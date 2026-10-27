// Facts about one translation unit, as clang parses it, for generate.py to merge into a call graph.
//
//   native-call-graph routines   DB REQUESTS ROOT [PREFIX=PATH]...
//   native-call-graph overriders DB NEEDS    ROOT [PREFIX=PATH]...
//
// DB is a compilation database; every command in it is parsed, exactly as written, except that
// it only checks syntax, and that a precompiled header it names is ignored in favour of the
// header the command also includes. ROOT is the source tree that paths are written relative to;
// each PREFIX=PATH names another tree, whose paths are written as PREFIX/relative. A location in
// no tree is an error, as is any diagnostic error in a unit. Facts go to standard output, one per
// line, tab-separated:
//
// `routines` reads REQUESTS (lines of key, path relative to ROOT, and symbol) and writes, for each
// function defined in the file at that path whose qualified name is the symbol or ends in :: and
// the symbol,
//   routine  KEY USR NAME SIGNATURE PATH:LINE   a definition of the requested routine, or of an
//                                               instantiation of a requested template
//   template KEY                                a requested template's own, uninstantiated body
//   call     ROUTINE-USR PATH:LINE MACRO KIND CALLEE-NAME CALLEE-SIGNATURE CALLEE-USR ENUMS RECEIVER-USR
//   checked  ROUTINE-USR                        clang's IR for the routine's definition in this
//                                               unit calls what the walk found, and nothing else
// where KIND is direct, dispatch (a virtual call, to be resolved by generate.py against the
// receiver's class), dispatch-delete (a delete-expression's call of a virtual destructor, which
// runs the deleting destructor), indirect, construct, destruct, new, delete, builtin, tls-init,
// throw or asm. It compiles the unit to IR as it walks it, without optimisation and with a line
// table, and fails on any routine whose IR calls, on some line, something the walk does not name
// there, or does not call something the walk names there.
//
// `overriders` reads NEEDS (lines of a receiver class's USR, a virtual method's USR, and call or
// delete) and writes
//   overrider        RECEIVER-USR METHOD-USR VARIANT CLASS-USR NAME SIGNATURE USR
//   overrider-delete RECEIVER-USR METHOD-USR VARIANT CLASS-USR NAME SIGNATURE USR
// for each class the unit completes that is or derives from the receiver class, naming the
// function in the vtable slot the call loads through each subobject of the receiver's class, and
// for a deleting call, the operator delete that function frees by. An abstract class counts: a
// call made while it is under construction or destruction reaches its own functions.

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/VTableBuilder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/ArgumentsAdjusters.h"
#include "clang/Tooling/JSONCompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
#include <fstream>
#include <functional>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using namespace clang;

namespace {

[[noreturn]] void fail(const std::string &Message) {
  llvm::errs() << "native-call-graph: " << Message << "\n";
  std::exit(2);
}

std::vector<std::vector<std::string>> readTsv(const std::string &Path, size_t Fields) {
  std::ifstream In(Path);
  if (!In)
    fail("cannot read " + Path);
  std::vector<std::vector<std::string>> Rows;
  std::string Line;
  while (std::getline(In, Line)) {
    if (Line.empty())
      continue;
    std::vector<std::string> Row;
    std::stringstream S(Line);
    std::string Field;
    while (std::getline(S, Field, '\t'))
      Row.push_back(Field);
    if (Row.size() != Fields)
      fail(Path + ": expected " + std::to_string(Fields) + " fields in: " + Line);
    Rows.push_back(std::move(Row));
  }
  return Rows;
}

std::string realPath(llvm::StringRef Path) {
  llvm::SmallString<256> Out;
  if (llvm::sys::fs::real_path(Path, Out))
    fail("cannot resolve " + Path.str());
  return std::string(Out);
}

/// The trees that paths are written relative to: the root, written bare, and each prefix.
struct Trees {
  std::vector<std::pair<std::string, std::string>> Prefixes; // (written prefix, real path)

  /// Path, relative to the tree that holds it.
  std::string relative(llvm::StringRef Path) const {
    std::string Real = realPath(Path);
    for (const auto &[Written, Tree] : Prefixes) {
      if (Real.size() > Tree.size() && Real.compare(0, Tree.size(), Tree) == 0 && Real[Tree.size()] == '/') {
        std::string Rest = Real.substr(Tree.size() + 1);
        return Written.empty() ? Rest : Written + "/" + Rest;
      }
    }
    fail("a location in no named tree: " + Real);
  }
};

PrintingPolicy policy(const ASTContext &Context) {
  PrintingPolicy P(Context.getLangOpts());
  P.SuppressTagKeyword = true;
  P.AnonymousTagLocations = false;
  P.FullyQualifiedName = true;
  P.SuppressDefaultTemplateArgs = false;
  return P;
}

std::string usrOf(const Decl *D) {
  llvm::SmallString<128> Buf;
  if (index::generateUSRForDecl(D, Buf))
    fail("no USR for a declaration");
  return std::string(Buf);
}

std::string nameOf(const NamedDecl *D, const PrintingPolicy &P) {
  std::string S;
  llvm::raw_string_ostream OS(S);
  D->getNameForDiagnostic(OS, P, /*Qualified=*/true);
  return S;
}

std::string typeOf(QualType T, const PrintingPolicy &P) { return T.getCanonicalType().getAsString(P); }

/// Whether a request's symbol names a function: the function's name qualified fully, as the text
/// of its definition rather than an instantiation of it names it, or that name's end after a ::.
bool names(const std::string &Symbol, const FunctionDecl *Written) {
  std::string Qualified = Written->getQualifiedNameAsString();
  if (Qualified == Symbol)
    return true;
  std::string Suffix = "::" + Symbol;
  return Qualified.size() > Suffix.size() &&
         Qualified.compare(Qualified.size() - Suffix.size(), Suffix.size(), Suffix) == 0;
}

/// The last component of a qualified name.
std::string lastComponent(const std::string &Symbol) {
  size_t Colons = Symbol.rfind("::");
  return Colons == std::string::npos ? Symbol : Symbol.substr(Colons + 2);
}

class Locations {
public:
  Locations(const SourceManager &SM, const LangOptions &LO, Preprocessor &PP, const Trees &T)
      : SM(SM), LO(LO), PP(PP), T(T) {}

  /// The file and line of a location, as the text that the reader of a file sees it: a token a
  /// macro's body supplies is at the outermost macro's invocation, and a token a macro's argument
  /// supplies is where the argument is written.
  std::string line(SourceLocation Loc) const {
    SourceLocation File = SM.getFileLoc(Loc);
    if (File.isInvalid())
      fail("an invalid location");
    return fileOf(File) + ":" + std::to_string(SM.getSpellingLineNumber(File));
  }

  /// The outermost macro whose body supplies the token at Loc, as PATH:NAME of its definition,
  /// or - when the token is written in a file or in a macro argument written in a file.
  std::string outermostMacro(SourceLocation Loc) const {
    std::string Outermost = "-";
    while (Loc.isMacroID()) {
      if (SM.isMacroArgExpansion(Loc)) {
        Loc = SM.getImmediateSpellingLoc(Loc);
        continue;
      }
      SourceLocation Invocation = SM.getImmediateExpansionRange(Loc).getBegin();
      // A token that ## pastes together is spelled in scratch space, at an expansion of its own
      // whose range is the operands of ##, in the body of the macro that pastes it.
      if (SM.isWrittenInScratchSpace(SM.getImmediateSpellingLoc(Loc))) {
        Loc = Invocation;
        continue;
      }
      llvm::StringRef Name = Lexer::getImmediateMacroName(Loc, SM, LO);
      IdentifierInfo *II = PP.getIdentifierInfo(Name);
      MacroDefinition Def = PP.getMacroDefinitionAtLoc(II, SM.getExpansionLoc(Invocation));
      MacroInfo *MI = Def.getMacroInfo();
      if (!MI)
        fail("no definition of the macro " + Name.str() + " at " + line(Invocation));
      Outermost = fileOf(MI->getDefinitionLoc()) + ":" + Name.str();
      Loc = Invocation;
    }
    return Outermost;
  }

  std::string fileOf(SourceLocation FileLoc) const {
    FileID Id = SM.getFileID(SM.getFileLoc(FileLoc));
    auto Found = Cache.find(Id);
    if (Found != Cache.end())
      return Found->second;
    OptionalFileEntryRef Entry = SM.getFileEntryRefForID(Id);
    if (!Entry)
      fail("a location in no file");
    std::string Rel = T.relative(Entry->getName());
    Cache.emplace(Id, Rel);
    return Rel;
  }

private:
  const SourceManager &SM;
  const LangOptions &LO;
  Preprocessor &PP;
  const Trees &T;
  mutable std::map<FileID, std::string> Cache;
};

const CXXRecordDecl *recordOf(QualType T) {
  T = T.getCanonicalType();
  if (const auto *Ptr = T->getAs<PointerType>())
    T = Ptr->getPointeeType();
  return T->getAsCXXRecordDecl();
}

/// The receiver as its own expression types it, before any conversion to a base class.
const Expr *receiverOf(const Expr *E) {
  while (true) {
    E = E->IgnoreParens();
    if (const auto *Cast = dyn_cast<ImplicitCastExpr>(E)) {
      switch (Cast->getCastKind()) {
      case CK_DerivedToBase:
      case CK_UncheckedDerivedToBase:
      case CK_NoOp:
      case CK_LValueToRValue:
        E = Cast->getSubExpr();
        continue;
      default:
        return E;
      }
    }
    return E;
  }
}

const CXXDestructorDecl *nonTrivialDestructor(QualType T, ASTContext &Context) {
  const CXXRecordDecl *RD = Context.getBaseElementType(T)->getAsCXXRecordDecl();
  if (!RD || !RD->hasDefinition() || RD->hasTrivialDestructor())
    return nullptr;
  const CXXDestructorDecl *D = RD->getDestructor();
  if (!D)
    fail("a class with a non-trivial destructor declares none: " + RD->getQualifiedNameAsString());
  return D;
}

/// Whether a statement holds a label a goto or a switch outside it could reach, which keeps clang
/// from leaving out a branch that holds it.
bool containsLabel(const Stmt *S, bool IgnoreCases = false) {
  if (!S)
    return false;
  if (isa<LabelStmt>(S) || (isa<SwitchCase>(S) && !IgnoreCases))
    return true;
  if (isa<SwitchStmt>(S))
    IgnoreCases = true;
  for (const Stmt *Child : S->children())
    if (containsLabel(Child, IgnoreCases))
      return true;
  return false;
}

/// Whether a variable's initializer is evaluated as a constant rather than run: one of static or
/// thread storage whose initialization is constant; or a local whose value constant expressions
/// could use and whose initializer evaluates, as a constexpr variable's or a const int's does,
/// which clang stores as a constant.
bool initializedAsConstant(VarDecl *V, const ASTContext &Context) {
  if (!V->hasLocalStorage())
    return V->hasConstantInitialization();
  return V->mightBeUsableInConstantExpressions(Context) && V->evaluateValue();
}

/// Whether a function of this type may throw.
bool typeMayThrow(QualType T) {
  T = T.getCanonicalType();
  if (const auto *Ptr = T->getAs<PointerType>())
    T = Ptr->getPointeeType();
  else if (const auto *MemPtr = T->getAs<MemberPointerType>())
    T = MemPtr->getPointeeType();
  const auto *Proto = T->getAs<FunctionProtoType>();
  return !Proto || !Proto->isNothrow();
}

bool functionMayThrow(const FunctionDecl *F) {
  return !F->hasAttr<NoThrowAttr>() && typeMayThrow(F->getType());
}

/// Whether evaluating a statement may throw: whether it calls, constructs with or allocates by a
/// function that may throw, or throws, in a language with exceptions.
class ThrowFinder : public RecursiveASTVisitor<ThrowFinder> {
public:
  static bool mayThrow(const Stmt *S, const ASTContext &Context) {
    if (!S || !Context.getLangOpts().CXXExceptions)
      return false;
    ThrowFinder Finder;
    Finder.TraverseStmt(const_cast<Stmt *>(S));
    return Finder.Found;
  }

  bool shouldVisitImplicitCode() const { return true; }
  bool TraverseUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *) { return true; }
  bool TraverseCXXNoexceptExpr(CXXNoexceptExpr *) { return true; }
  bool TraverseLambdaExpr(LambdaExpr *E) {
    for (Expr *Init : E->capture_inits())
      if (Init)
        TraverseStmt(Init);
    return true;
  }
  bool TraverseCXXDefaultArgExpr(CXXDefaultArgExpr *E) { return TraverseStmt(E->getExpr()); }
  bool TraverseCXXDefaultInitExpr(CXXDefaultInitExpr *E) { return TraverseStmt(E->getExpr()); }

  bool VisitCallExpr(CallExpr *E) {
    const auto *F = dyn_cast_or_null<FunctionDecl>(E->getCalleeDecl());
    if (F ? functionMayThrow(F) : typeMayThrow(E->getCallee()->getType()))
      Found = true;
    return true;
  }
  bool VisitCXXConstructExpr(CXXConstructExpr *E) {
    if (!E->getConstructor()->isTrivial() && functionMayThrow(E->getConstructor()))
      Found = true;
    return true;
  }
  bool VisitCXXNewExpr(CXXNewExpr *E) {
    if (E->getOperatorNew() && functionMayThrow(E->getOperatorNew()))
      Found = true;
    return true;
  }
  bool VisitCXXThrowExpr(CXXThrowExpr *) {
    Found = true;
    return true;
  }

private:
  bool Found = false;
};

/// The method a virtual call of Method on the object Base denotes calls directly, as clang emits
/// it: where the object's dynamic class is known, as a final class's or a local object's is, and
/// the method that class runs returns what Method does; or null.
const CXXMethodDecl *devirtualized(const CXXMethodDecl *Method, const Expr *Base) {
  if (!const_cast<CXXMethodDecl *>(Method)->getDevirtualizedMethod(Base, /*IsAppleKext=*/false))
    return nullptr;
  const CXXRecordDecl *Dynamic = Base->getBestDynamicClassType();
  const CXXMethodDecl *Known = Dynamic ? Method->getCorrespondingMethodInClass(Dynamic) : nullptr;
  if (!Known || Known->getReturnType().getCanonicalType() != Method->getReturnType().getCanonicalType())
    return nullptr;
  const CXXRecordDecl *Class = Known->getParent()->getCanonicalDecl();
  auto Is = [&](const Expr *E) {
    const CXXRecordDecl *R = recordOf(E->getType());
    return R && R->getCanonicalDecl() == Class;
  };
  return Is(Base->IgnoreParenBaseCasts()) || Is(Base) ? Known : nullptr;
}

/// What the walk of one routine's definition in one unit found it runs, in the terms clang's IR
/// for the routine can be compared with: each call by the file and line clang's debug information
/// gives it.
struct Walked {
  const FunctionDecl *Routine = nullptr;
  std::string Usr;
  /// Each function it calls directly, constructs or destroys by, or allocates or frees by, and each
  /// thread-local variable's wrapper it calls (as tls:USR), by line and USR, with a name for each.
  std::map<std::pair<std::string, std::string>, std::string> Calls;
  /// The same functions by USR alone, for the builtins clang lowers to an intrinsic.
  std::map<std::string, const FunctionDecl *> Callees;
  /// Each call through a pointer, virtual or not, and each inline assembly statement, by line, and
  /// whether only the complete object's constructor makes it, as a virtual base's initializer's.
  struct Site {
    std::string Line;
    bool CompleteOnly;
  };
  std::map<const Stmt *, Site> Indirect;
  std::map<const Stmt *, Site> Asm;
  bool Throws = false;
  bool Rethrows = false;
  bool Catches = false;
  /// Whether it initializes a static local at run time under a __cxa_guard, and whether it
  /// registers a static local's destructor to run at the process's exit or at its thread's.
  bool GuardsStatic = false;
  bool RegistersExit = false;
  bool RegistersThreadExit = false;
  /// The functions defined inside it, a lambda's or a local class's, whose code counts as its own.
  std::set<const FunctionDecl *> Nested;
};

/// Whether a variable is read through the wrapper Itanium's ABI gives a thread_local whose
/// initialization may run, as clang decides it: unless it is constant-initialized, wherever it is
/// defined, and has no destructor.
bool readThroughWrapper(const VarDecl *V, const ASTContext &Context) {
  if (V->getTLSKind() != VarDecl::TLS_Dynamic || V->isLocalVarDeclOrParm())
    return false;
  V = V->getMostRecentDecl();
  bool MayNeedDestruction = V->needsDestruction(Context) != QualType::DK_none;
  if (const Type *T = V->getType()->getBaseElementTypeUnsafe(); T->getAs<RecordType>() && T->isIncompleteType())
    MayNeedDestruction = true;
  bool Constant;
  if (V->hasAttr<ConstInitAttr>())
    Constant = true;
  else if (V->isWeak() || V->hasAttr<SelectAnyAttr>())
    Constant = false;
  else if (const VarDecl *Init = V->getInitializingDeclaration(); !Init)
    Constant = false;
  else if (!Init->hasInit())
    Constant = true;
  else {
    GVALinkage Linkage = const_cast<ASTContext &>(Context).GetGVALinkageForVariable(V);
    if (Linkage == GVA_Internal || Linkage == GVA_StrongExternal)
      Constant = !MayNeedDestruction && const_cast<VarDecl *>(Init)->evaluateValue();
    else
      Constant = Init->hasConstantInitialization();
  }
  return !Constant || MayNeedDestruction;
}

/// Whether clang's code for an expression statement ends where nothing follows: on a call of a
/// function that does not return, or a throw, with no branch after it to join. Answers false
/// where it cannot tell, which only keeps the walk from leaving out what follows.
bool endsWithoutReturning(const Expr *E, const ASTContext &Context) {
  while (true) {
    if (const auto *Full = dyn_cast<FullExpr>(E))
      E = Full->getSubExpr();
    else if (const auto *Paren = dyn_cast<ParenExpr>(E))
      E = Paren->getSubExpr();
    else if (const auto *Cast = dyn_cast<CastExpr>(E))
      E = Cast->getSubExpr();
    else if (const auto *Bind = dyn_cast<CXXBindTemporaryExpr>(E))
      E = Bind->getSubExpr();
    else if (const auto *Materialize = dyn_cast<MaterializeTemporaryExpr>(E))
      E = Materialize->getSubExpr();
    else
      break;
  }
  auto Branches = [](const Stmt *S) {
    std::function<bool(const Stmt *)> Any = [&](const Stmt *T) -> bool {
      if (!T)
        return false;
      if (isa<ConditionalOperator, BinaryConditionalOperator, StmtExpr, LambdaExpr>(T))
        return true;
      if (const auto *B = dyn_cast<BinaryOperator>(T); B && B->isLogicalOp())
        return true;
      for (const Stmt *Child : T->children())
        if (Any(Child))
          return true;
      return false;
    };
    return Any(S);
  };
  if (const auto *Throw = dyn_cast<CXXThrowExpr>(E))
    return Context.getLangOpts().CXXExceptions && !Branches(Throw->getSubExpr());
  if (const auto *Comma = dyn_cast<BinaryOperator>(E); Comma && Comma->getOpcode() == BO_Comma)
    return endsWithoutReturning(Comma->getRHS(), Context) ||
           (endsWithoutReturning(Comma->getLHS(), Context) && !Branches(Comma->getRHS()));
  const auto *Call = dyn_cast<CallExpr>(E);
  if (!Call)
    return false;
  for (const Expr *Arg : Call->arguments())
    if (Branches(Arg))
      return false;
  if (const FunctionDecl *F = Call->getDirectCallee()) {
    if (unsigned Builtin = F->getBuiltinID(); Builtin && !Context.BuiltinInfo.isPredefinedLibFunction(Builtin))
      return Builtin == Builtin::BI__builtin_unreachable;
    return F->isNoReturn();
  }
  QualType T = Call->getCallee()->getType();
  if (const auto *Ptr = T->getAs<PointerType>())
    T = Ptr->getPointeeType();
  const auto *Function = T->getAs<FunctionType>();
  return Function && Function->getNoReturnAttr();
}

/// Every call one routine's definition makes, of the code clang emits for it: a statement that
/// follows a return, a break, a continue, a goto, a loop with no way out, or an expression
/// statement that does not return, and that holds no label, is code clang emits nothing for.
class CallCollector : public RecursiveASTVisitor<CallCollector> {
public:
  CallCollector(ASTContext &Context, const Locations &L, Walked &W, std::set<std::string> &Out)
      : Context(Context), P(policy(Context)), L(L), RoutineUsr(W.Usr), W(W), Out(Out) {}

  bool shouldVisitImplicitCode() const { return true; }
  bool shouldVisitTemplateInstantiations() const { return true; }

  void collect(const FunctionDecl *F) {
    SourceLocation End = F->getBody()->getEndLoc();
    if (const auto *Ctor = dyn_cast<CXXConstructorDecl>(F)) {
      llvm::SmallVector<const CXXCtorInitializer *, 8> Inits(Ctor->init_begin(), Ctor->init_end());
      for (const CXXCtorInitializer *Init : Inits) {
        // A virtual base's initializer runs only in the complete object's constructor.
        bool Saved = InVirtualBaseInit;
        InVirtualBaseInit = Init->isBaseInitializer() && Init->isBaseVirtual();
        TraverseStmt(Init->getInit());
        InVirtualBaseInit = Saved;
      }
      // If a later initializer or the body throws, what the constructor has constructed is
      // destroyed.
      bool BodyThrows = ThrowFinder::mayThrow(F->getBody(), Context);
      for (size_t I = 0; I < Inits.size(); ++I) {
        bool LaterThrows = BodyThrows;
        for (size_t J = I + 1; J < Inits.size() && !LaterThrows; ++J)
          LaterThrows = ThrowFinder::mayThrow(Inits[J]->getInit(), Context);
        if (!LaterThrows)
          continue;
        QualType Constructed;
        if (Inits[I]->isDelegatingInitializer())
          Constructed = Context.getRecordType(Ctor->getParent());
        else if (const Type *Base = Inits[I]->getBaseClass())
          Constructed = QualType(Base, 0);
        else if (const FieldDecl *Field = Inits[I]->getAnyMember())
          Constructed = Field->getParent()->isUnion() ? QualType() : Field->getType();
        if (!Constructed.isNull())
          if (const CXXDestructorDecl *D = nonTrivialDestructor(Constructed, Context)) {
            bool Saved = InVirtualBaseInit;
            InVirtualBaseInit = Inits[I]->isBaseInitializer() && Inits[I]->isBaseVirtual();
            emit(End, End, "destruct", D, "-");
            InVirtualBaseInit = Saved;
          }
      }
    }
    for (const ParmVarDecl *Param : F->parameters()) {
      const CXXRecordDecl *RD = Param->getType()->getAsCXXRecordDecl();
      if (RD && RD->isParamDestroyedInCallee())
        if (const CXXDestructorDecl *D = nonTrivialDestructor(Param->getType(), Context))
          emit(Param->getLocation(), End, "destruct", D, "-");
    }
    statement(F->getBody());
    if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(F))
      destroyBasesAndMembers(Dtor->getParent(), End);
  }

  // Recursion rather than RecursiveASTVisitor's queue, so that Enclosing brackets every child.
  // A statement clang reaches no code for is skipped, unless it holds a label; one that holds a
  // label is reached by it.
  bool TraverseStmt(Stmt *S, DataRecursionQueue * = nullptr) {
    if (!S)
      return true;
    if (!Reachable && !isa<CompoundStmt, LabelStmt, AttributedStmt, SwitchCase, GotoStmt, BreakStmt, ContinueStmt,
                           NullStmt>(S)) {
      if (!containsLabel(S))
        return true;
      Reachable = true;
    }
    bool Located = S->getBeginLoc().isValid();
    if (Located)
      Enclosing.push_back(S->getBeginLoc());
    bool Result = RecursiveASTVisitor::TraverseStmt(S, nullptr);
    if (Located)
      Enclosing.pop_back();
    return Result;
  }

  bool TraverseCompoundStmt(CompoundStmt *S) {
    Scopes.push_back(S->getRBracLoc());
    for (Stmt *Child : S->body())
      statement(Child);
    Scopes.pop_back();
    return true;
  }
  bool TraverseLabelStmt(LabelStmt *S) {
    Reachable = true;
    return statement(S->getSubStmt());
  }
  bool TraverseCaseStmt(CaseStmt *S) {
    Reachable = true;
    return statement(S->getSubStmt());
  }
  bool TraverseDefaultStmt(DefaultStmt *S) {
    Reachable = true;
    return statement(S->getSubStmt());
  }
  bool TraverseAttributedStmt(AttributedStmt *S) { return statement(S->getSubStmt()); }
  bool TraverseReturnStmt(ReturnStmt *S) {
    bool Result = RecursiveASTVisitor::TraverseReturnStmt(S);
    Reachable = false;
    return Result;
  }
  bool TraverseGotoStmt(GotoStmt *) {
    Reachable = false;
    return true;
  }
  bool TraverseIndirectGotoStmt(IndirectGotoStmt *S) {
    bool Result = RecursiveASTVisitor::TraverseIndirectGotoStmt(S);
    Reachable = false;
    return Result;
  }
  bool TraverseBreakStmt(BreakStmt *) {
    if (Reachable && !Broke.empty())
      Broke.back() = true;
    Reachable = false;
    return true;
  }
  bool TraverseContinueStmt(ContinueStmt *) {
    Reachable = false;
    return true;
  }
  bool TraverseWhileStmt(WhileStmt *S) {
    Scopes.push_back(S->getBeginLoc());
    TraverseStmt(S->getConditionVariableDeclStmt());
    TraverseStmt(S->getCond());
    bool Exits = loop(S->getBody());
    Scopes.pop_back();
    // A condition clang folds to true has no edge out of the loop.
    std::optional<bool> Folded = folded(S->getCond());
    Reachable = Exits || !(Folded && *Folded);
    return true;
  }
  bool TraverseForStmt(ForStmt *S) {
    Scopes.push_back(S->getBeginLoc());
    statement(S->getInit());
    TraverseStmt(S->getConditionVariableDeclStmt());
    TraverseStmt(S->getCond());
    bool Exits = loop(S->getBody());
    Reachable = true;
    TraverseStmt(S->getInc());
    Scopes.pop_back();
    std::optional<bool> Folded = S->getCond() ? folded(S->getCond()) : std::optional<bool>(true);
    Reachable = Exits || !(Folded && *Folded);
    return true;
  }
  bool TraverseDoStmt(DoStmt *S) {
    loop(S->getBody());
    Reachable = true;
    TraverseStmt(S->getCond());
    Reachable = true;
    return true;
  }
  bool TraverseCXXForRangeStmt(CXXForRangeStmt *S) {
    Scopes.push_back(S->getBeginLoc());
    statement(S->getInit());
    statement(S->getRangeStmt());
    statement(S->getBeginStmt());
    statement(S->getEndStmt());
    TraverseStmt(S->getCond());
    statement(S->getLoopVarStmt());
    loop(S->getBody());
    Reachable = true;
    TraverseStmt(S->getInc());
    Scopes.pop_back();
    Reachable = true;
    return true;
  }
  bool TraverseSwitchStmt(SwitchStmt *S) {
    Scopes.push_back(S->getBeginLoc());
    statement(S->getInit());
    TraverseStmt(S->getConditionVariableDeclStmt());
    TraverseStmt(S->getCond());
    // The switch's own body is reached only through its labels.
    Reachable = false;
    Broke.push_back(false);
    statement(S->getBody());
    bool Exits = Broke.back() || Reachable || !hasDefault(S->getBody());
    Broke.pop_back();
    Scopes.pop_back();
    Reachable = Exits;
    return true;
  }
  bool TraverseCXXTryStmt(CXXTryStmt *S) {
    statement(S->getTryBlock());
    bool Continues = Reachable;
    for (unsigned I = 0; I < S->getNumHandlers(); ++I) {
      Reachable = true;
      TraverseStmt(S->getHandler(I));
      Continues = Continues || Reachable;
    }
    Reachable = Continues;
    return true;
  }
  bool TraverseCXXCatchStmt(CXXCatchStmt *S) {
    W.Catches = true;
    // The caught object's copy is destroyed at the end of the handler's block.
    const auto *Block = dyn_cast<CompoundStmt>(S->getHandlerBlock());
    Scopes.push_back(Block ? Block->getRBracLoc() : S->getEndLoc());
    if (VarDecl *Caught = S->getExceptionDecl())
      TraverseDecl(Caught);
    Scopes.pop_back();
    return statement(S->getHandlerBlock());
  }

  // Unevaluated operands and compile-time evaluation run nothing. The operand of sizeof is
  // evaluated only when its type is a variable-length array's, and a type's bounds are then
  // what run.
  bool TraverseUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E) {
    if (E->getKind() != UETT_SizeOf || !E->getTypeOfArgument()->isVariablyModifiedType())
      return true;
    if (!E->isArgumentType())
      return TraverseStmt(E->getArgumentExpr());
    for (QualType T = E->getArgumentType(); const ArrayType *Array = Context.getAsArrayType(T);
         T = Array->getElementType())
      if (const auto *Variable = dyn_cast<VariableArrayType>(Array))
        if (!TraverseStmt(Variable->getSizeExpr()))
          return false;
    return true;
  }
  // A variable whose initializer is a constant, as a constexpr variable's is, or a variable of
  // static storage whose initialization is constant, is initialized without running it.
  bool TraverseVarDecl(VarDecl *V) {
    if (V->getInit() && initializedAsConstant(V, Context)) {
      VisitVarDecl(V);
      return true;
    }
    return RecursiveASTVisitor::TraverseVarDecl(V);
  }
  // A parameter's default argument runs where a call uses it, as a CXXDefaultArgExpr.
  bool TraverseParmVarDecl(ParmVarDecl *) { return true; }
  // A function defined inside the routine, a local class's, is collected as the routine is.
  bool TraverseDecl(Decl *D) {
    if (auto *F = dyn_cast_or_null<FunctionDecl>(D); F && F != W.Routine) {
      W.Nested.insert(F);
      nested(F);
      return true;
    }
    return RecursiveASTVisitor::TraverseDecl(D);
  }
  // A lambda runs its captures' initializers where it is made, and its body, or for a generic
  // lambda each instantiation of its body, where it is called; its pattern runs nothing.
  bool TraverseLambdaExpr(LambdaExpr *E) {
    for (Expr *Init : E->capture_inits())
      if (Init && !TraverseStmt(Init))
        return false;
    for (Decl *Member : E->getLambdaClass()->decls()) {
      if (auto *Function = dyn_cast<FunctionDecl>(Member))
        W.Nested.insert(Function);
      else if (auto *Template = dyn_cast<FunctionTemplateDecl>(Member))
        for (FunctionDecl *Instance : Template->specializations())
          W.Nested.insert(Instance);
    }
    if (FunctionTemplateDecl *Template = E->getDependentCallOperator()) {
      for (FunctionDecl *Instance : Template->specializations())
        nested(Instance);
      return true;
    }
    nested(E->getCallOperator());
    return true;
  }
  bool TraverseCXXNoexceptExpr(CXXNoexceptExpr *) { return true; }
  bool TraverseDecltypeTypeLoc(DecltypeTypeLoc, bool = false) { return true; }
  bool TraverseTypeOfExprTypeLoc(TypeOfExprTypeLoc, bool = false) { return true; }
  bool TraverseStaticAssertDecl(StaticAssertDecl *) { return true; }
  bool TraverseConstantExpr(ConstantExpr *) { return true; }
  bool TraverseCXXTypeidExpr(CXXTypeidExpr *E) {
    return E->isPotentiallyEvaluated() ? RecursiveASTVisitor::TraverseCXXTypeidExpr(E) : true;
  }
  // A temporary is destroyed at the end of its full-expression, at the line the full-expression
  // starts on.
  bool TraverseExprWithCleanups(ExprWithCleanups *E) {
    FullExpressions.push_back(E->getBeginLoc());
    bool Result = RecursiveASTVisitor::TraverseExprWithCleanups(E);
    FullExpressions.pop_back();
    return Result;
  }
  // clang emits only the branch a condition it folds to a constant takes, unless the other holds a
  // label a goto could reach; the condition itself is then not emitted either.
  bool TraverseConditionalOperator(ConditionalOperator *E) {
    if (std::optional<bool> Folded = folded(E->getCond())) {
      Expr *Dead = *Folded ? E->getFalseExpr() : E->getTrueExpr();
      if (!containsLabel(Dead))
        return TraverseStmt(*Folded ? E->getTrueExpr() : E->getFalseExpr());
    }
    return RecursiveASTVisitor::TraverseConditionalOperator(E);
  }
  bool TraverseBinaryOperator(BinaryOperator *E) {
    if (E->getOpcode() == BO_LAnd || E->getOpcode() == BO_LOr)
      if (std::optional<bool> Folded = folded(E->getLHS())) {
        bool Decides = *Folded == (E->getOpcode() == BO_LOr);
        if (!Decides)
          return TraverseStmt(E->getRHS());
        if (!containsLabel(E->getRHS()))
          return true;
      }
    return RecursiveASTVisitor::TraverseBinaryOperator(E);
  }
  bool TraverseIfStmt(IfStmt *S) {
    Scopes.push_back(S->getBeginLoc());
    statement(S->getInit());
    TraverseStmt(S->getConditionVariableDeclStmt());
    std::optional<Stmt *> Taken;
    if (S->isConstexpr()) {
      Taken = S->getNondiscardedCase(Context);
      if (!Taken)
        fail("an if constexpr whose condition is value-dependent at " + L.line(S->getIfLoc()));
    } else if (std::optional<bool> Folded = folded(S->getCond())) {
      if (!containsLabel(*Folded ? S->getElse() : S->getThen()))
        Taken = *Folded ? S->getThen() : S->getElse();
    }
    if (Taken) {
      statement(*Taken);
    } else {
      TraverseStmt(S->getCond());
      statement(S->getThen());
      bool ThenContinues = Reachable;
      Reachable = true;
      statement(S->getElse());
      Reachable = Reachable || ThenContinues;
    }
    Scopes.pop_back();
    return true;
  }

  // A default argument runs where it is used, and clang gives its calls the line of the call that
  // uses it; a default member initializer's calls keep their own lines.
  bool TraverseCXXDefaultArgExpr(CXXDefaultArgExpr *E) {
    std::optional<SourceLocation> Saved = IrOverride;
    if (!IrOverride)
      IrOverride = E->getUsedLocation();
    bool Result = within(E->getUsedLocation(), E->getExpr());
    IrOverride = Saved;
    return Result;
  }
  bool TraverseCXXDefaultInitExpr(CXXDefaultInitExpr *E) { return within(E->getUsedLocation(), E->getExpr()); }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (const auto *V = dyn_cast<VarDecl>(E->getDecl()); V && readThroughWrapper(V, Context)) {
      std::string Usr = usrOf(V);
      std::string At = row(E->getLocation(), "tls-init", nameOf(V, P), typeOf(V->getType(), P), Usr, "-", "-");
      W.Calls.emplace(std::make_pair(irLine(E->getExprLoc()), "tls:" + Usr), nameOf(V, P) + " (at " + At + ")");
    }
    return true;
  }

  bool VisitCallExpr(CallExpr *E) {
    if (E->isInstantiationDependent())
      fail("a call in code that depends on a template parameter at " + L.line(E->getBeginLoc()));
    const Expr *Callee = E->getCallee()->IgnoreParenImpCasts();
    if (isa<CXXPseudoDestructorExpr>(Callee))
      return true;
    SourceLocation Loc = E->getBeginLoc();
    if (const auto *M = dyn_cast<MemberExpr>(Callee))
      Loc = M->getMemberLoc();
    else if (const auto *Op = dyn_cast<CXXOperatorCallExpr>(E))
      Loc = Op->getOperatorLoc();
    else if (const auto *Ref = dyn_cast<DeclRefExpr>(Callee))
      Loc = Ref->getLocation();
    SourceLocation IrLoc = E->getExprLoc();

    unsigned FirstArg = 0;
    const auto *F = dyn_cast_or_null<FunctionDecl>(E->getCalleeDecl());
    if (const auto *Member = dyn_cast<CXXMemberCallExpr>(E))
      F = Member->getMethodDecl();
    if (!F) {
      // A call through a pointer to a member function is typed by the pointer, not by the
      // bound member it makes.
      QualType Through = E->getCallee()->getType();
      if (const auto *Bound = dyn_cast<BinaryOperator>(E->getCallee()->IgnoreParens()); Bound && Bound->isPtrMemOp())
        Through = Bound->getRHS()->getType();
      emitIndirect(Loc, Through, enums(E, 0));
      site(W.Indirect, E, IrLoc);
      return true;
    }
    // A builtin that is not a library function, or that is one of std's, as std::move is, is
    // lowered by clang, and calls nothing.
    if (unsigned Builtin = F->getBuiltinID();
        Builtin && (!Context.BuiltinInfo.isPredefinedLibFunction(Builtin) || Context.BuiltinInfo.isInStdNamespace(Builtin))) {
      emit(Loc, IrLoc, "builtin", F, enums(E, 0));
      return true;
    }
    // A trivial function, as a trivial assignment operator is, copies bits, and calls nothing.
    if (F->isTrivial())
      return true;
    const auto *Method = dyn_cast<CXXMethodDecl>(F);
    const Expr *Receiver = nullptr;
    if (const auto *Member = dyn_cast<CXXMemberCallExpr>(E))
      Receiver = Member->getImplicitObjectArgument();
    else if (isa<CXXOperatorCallExpr>(E) && Method && Method->isInstance()) {
      Receiver = E->getArg(0);
      FirstArg = 1;
    }
    const auto *ME = dyn_cast<MemberExpr>(Callee);
    bool Qualified = ME && ME->hasQualifier();
    if (Method && Method->isVirtual() && !Qualified) {
      if (!Receiver)
        fail("a virtual call with no receiver at " + L.line(Loc));
      // clang calls directly what it can tell the receiver's dynamic class runs.
      const Expr *Base = ME ? ME->getBase() : Receiver;
      if (const CXXMethodDecl *Known = devirtualized(Method, Base)) {
        emit(Loc, IrLoc, "direct", Known, enums(E, FirstArg));
        return true;
      }
      const CXXRecordDecl *Class = recordOf(receiverOf(Receiver)->getType());
      if (!Class)
        fail("a virtual call whose receiver is not of a class at " + L.line(Loc));
      emit(Loc, IrLoc, "dispatch", F, enums(E, FirstArg), usrOf(Class->getDefinition()));
      site(W.Indirect, E, IrLoc);
      return true;
    }
    emit(Loc, IrLoc, "direct", F, enums(E, FirstArg));
    return true;
  }

  bool VisitCXXConstructExpr(CXXConstructExpr *E) {
    const CXXConstructorDecl *Ctor = E->getConstructor();
    // clang elides a copy the language lets it, and constructs the copied object in place.
    if (E->isElidable() && Context.getLangOpts().ElideConstructors)
      return true;
    if (!Ctor->isTrivial())
      emit(E->getLocation(), E->getExprLoc(), "construct", Ctor, enums(E));
    return true;
  }

  bool VisitCXXInheritedCtorInitExpr(CXXInheritedCtorInitExpr *E) {
    emit(E->getLocation(), E->getExprLoc(), "construct", E->getConstructor(), "-");
    return true;
  }

  bool VisitCXXNewExpr(CXXNewExpr *E) {
    if (!E->getOperatorNew())
      fail("a new expression with no operator new at " + L.line(E->getBeginLoc()));
    emit(E->getBeginLoc(), E->getExprLoc(), "new", E->getOperatorNew(), "-");
    // If the initializer throws, the matching deallocation function frees the memory.
    const FunctionDecl *Cleanup = E->getOperatorDelete();
    if (Cleanup && !Cleanup->isReservedGlobalPlacementOperator() &&
        ThrowFinder::mayThrow(E->getInitializer(), Context))
      emit(E->getBeginLoc(), E->getExprLoc(), "delete", Cleanup, "-");
    return true;
  }

  bool VisitCXXDeleteExpr(CXXDeleteExpr *E) {
    if (E->getDestroyedType()->isDependentType())
      fail("a delete of a dependent type at " + L.line(E->getBeginLoc()));
    SourceLocation IrLoc = E->getExprLoc();
    if (const CXXDestructorDecl *D = nonTrivialDestructor(E->getDestroyedType(), Context)) {
      // The dynamic class's deleting destructor runs its destructor and frees the object by
      // that class's operator delete.
      // clang destroys directly an object whose dynamic class it can tell is the static one.
      bool Virtual = D->isVirtual() && !E->isArrayForm();
      if (Virtual)
        if (const auto *Known = dyn_cast_or_null<CXXDestructorDecl>(
                const_cast<CXXDestructorDecl *>(D)->getDevirtualizedMethod(E->getArgument(), false));
            Known && Known->getParent()->getCanonicalDecl() == recordOf(E->getArgument()->getType())->getCanonicalDecl()) {
          D = Known;
          Virtual = false;
        }
      if (Virtual && !E->isGlobalDelete()) {
        emit(E->getBeginLoc(), IrLoc, "dispatch-delete", D, "-", usrOf(D->getParent()->getDefinition()));
        site(W.Indirect, E, IrLoc);
        return true;
      }
      if (Virtual) {
        emit(E->getBeginLoc(), IrLoc, "dispatch", D, "-", usrOf(D->getParent()->getDefinition()));
        site(W.Indirect, E, IrLoc);
      } else
        emit(E->getBeginLoc(), IrLoc, "destruct", D, "-");
    }
    if (!E->getOperatorDelete())
      fail("a delete expression with no operator delete at " + L.line(E->getBeginLoc()));
    emit(E->getBeginLoc(), IrLoc, "delete", E->getOperatorDelete(), "-");
    return true;
  }

  bool VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *E) {
    SourceLocation End = FullExpressions.empty() ? E->getBeginLoc() : FullExpressions.back();
    emit(E->getBeginLoc(), End, "destruct", E->getTemporary()->getDestructor(), "-");
    return true;
  }

  bool VisitVarDecl(VarDecl *V) {
    if (V->isStaticLocal()) {
      // A static thread_local has a guard of its own in thread-local storage, with no __cxa_guard.
      bool ThreadLocal = V->getTLSKind() != VarDecl::TLS_None;
      if (V->getInit() && !initializedAsConstant(V, Context) && !ThreadLocal)
        W.GuardsStatic = true;
      if (nonTrivialDestructor(V->getType(), Context))
        (ThreadLocal ? W.RegistersThreadExit : W.RegistersExit) = true;
      return true;
    }
    if (isa<ParmVarDecl>(V) || !V->hasLocalStorage())
      return true;
    if (V->getType()->isReferenceType())
      return true;
    if (const CXXDestructorDecl *D = nonTrivialDestructor(V->getType(), Context))
      emit(V->getLocation(), Scopes.empty() ? V->getLocation() : Scopes.back(), "destruct", D, "-");
    return true;
  }

  bool VisitCXXThrowExpr(CXXThrowExpr *E) {
    const Expr *Thrown = E->getSubExpr();
    std::string What = Thrown ? typeOf(Thrown->getType(), P) : "-";
    row(E->getThrowLoc(), "throw", What, "-", "-", "-", "-");
    (Thrown ? W.Throws : W.Rethrows) = true;
    return true;
  }

  bool VisitAsmStmt(AsmStmt *S) {
    row(S->getAsmLoc(), "asm", "-", "-", "-", "-", "-");
    site(W.Asm, S, S->getAsmLoc());
    return true;
  }

private:
  /// Traverses a statement where a statement stands, and leaves the rest unreached after an
  /// expression statement that does not return.
  bool statement(Stmt *S) {
    bool Result = TraverseStmt(S);
    if (Reachable)
      if (const auto *E = dyn_cast_or_null<Expr>(S); E && endsWithoutReturning(E, Context))
        Reachable = false;
    return Result;
  }

  /// Traverses a loop's body, which the loop's head reaches, and answers whether a break the body
  /// reaches leaves the loop.
  bool loop(Stmt *Body) {
    Broke.push_back(false);
    Reachable = true;
    statement(Body);
    bool Exits = Broke.back();
    Broke.pop_back();
    return Exits;
  }

  /// Collects a function defined inside the routine, from its own start.
  void nested(const FunctionDecl *F) {
    if (!F->doesThisDeclarationHaveABody() || F->isDependentContext())
      return;
    bool SavedReachable = Reachable;
    std::vector<bool> SavedBroke = std::move(Broke);
    std::vector<SourceLocation> SavedScopes = std::move(Scopes);
    std::vector<SourceLocation> SavedFull = std::move(FullExpressions);
    std::optional<SourceLocation> SavedUse = UseOverride, SavedIr = IrOverride;
    bool SavedVirtualBase = InVirtualBaseInit;
    Reachable = true;
    Broke.clear();
    Scopes.clear();
    FullExpressions.clear();
    UseOverride.reset();
    IrOverride.reset();
    InVirtualBaseInit = false;
    collect(F);
    Reachable = SavedReachable;
    Broke = std::move(SavedBroke);
    Scopes = std::move(SavedScopes);
    FullExpressions = std::move(SavedFull);
    UseOverride = SavedUse;
    IrOverride = SavedIr;
    InVirtualBaseInit = SavedVirtualBase;
  }

  /// Whether a switch's body has a default label of its own, not a nested switch's.
  static bool hasDefault(const Stmt *S) {
    if (!S || isa<SwitchStmt>(S))
      return false;
    if (isa<DefaultStmt>(S))
      return true;
    for (const Stmt *Child : S->children())
      if (hasDefault(Child))
        return true;
    return false;
  }

  /// The value of a condition clang folds to a constant, as it does when it evaluates as an
  /// integer without side effects and holds no label.
  std::optional<bool> folded(const Expr *Cond) {
    Expr::EvalResult Result;
    if (!Cond || Cond->isValueDependent() || !Cond->EvaluateAsInt(Result, Context) || containsLabel(Cond))
      return std::nullopt;
    return Result.Val.getInt().getBoolValue();
  }

  bool within(SourceLocation Use, Expr *E) {
    std::optional<SourceLocation> Saved = UseOverride;
    if (!UseOverride)
      UseOverride = Use;
    bool Result = TraverseStmt(E);
    UseOverride = Saved;
    return Result;
  }

  void destroyBasesAndMembers(const CXXRecordDecl *RD, SourceLocation Loc) {
    for (const CXXBaseSpecifier &Base : RD->bases())
      if (const CXXDestructorDecl *D = nonTrivialDestructor(Base.getType(), Context))
        emit(Loc, Loc, "destruct", D, "-");
    for (const CXXBaseSpecifier &Base : RD->vbases())
      if (const CXXDestructorDecl *D = nonTrivialDestructor(Base.getType(), Context))
        emit(Loc, Loc, "destruct", D, "-");
    if (RD->isUnion())
      return;
    for (const FieldDecl *Field : RD->fields())
      if (const CXXDestructorDecl *D = nonTrivialDestructor(Field->getType(), Context))
        emit(Loc, Loc, "destruct", D, "-");
  }

  /// The arguments that name an enumerator, as N=Enum::Enumerator, numbering from 1 at First.
  std::string enumsOf(llvm::ArrayRef<const Expr *> Args) {
    std::string S;
    for (size_t I = 0; I < Args.size(); ++I) {
      const Expr *A = Args[I];
      if (const auto *Default = dyn_cast<CXXDefaultArgExpr>(A))
        A = Default->getExpr();
      const auto *Ref = dyn_cast<DeclRefExpr>(A->IgnoreParenImpCasts());
      if (!Ref)
        continue;
      const auto *Constant = dyn_cast<EnumConstantDecl>(Ref->getDecl());
      if (!Constant)
        continue;
      const auto *Enum = cast<EnumDecl>(Constant->getDeclContext());
      if (!S.empty())
        S += "|";
      S += std::to_string(I + 1) + "=";
      if (Enum->getDeclName())
        S += nameOf(Enum, P) + "::";
      S += Constant->getNameAsString();
    }
    return S.empty() ? "-" : S;
  }

  std::string enums(const CallExpr *E, unsigned First) {
    std::vector<const Expr *> Args;
    for (unsigned I = First; I < E->getNumArgs(); ++I)
      Args.push_back(E->getArg(I));
    return enumsOf(Args);
  }

  std::string enums(const CXXConstructExpr *E) {
    std::vector<const Expr *> Args(E->arg_begin(), E->arg_end());
    return enumsOf(Args);
  }

  /// The file and line clang's debug information gives a call at Loc.
  std::string irLine(SourceLocation Loc) {
    SourceLocation At = IrOverride ? *IrOverride : Loc;
    if (At.isInvalid() && !Enclosing.empty())
      At = Enclosing.back();
    if (At.isInvalid())
      fail("no location for a call in " + RoutineUsr);
    return L.line(At);
  }

  void site(std::map<const Stmt *, Walked::Site> &Sites, const Stmt *S, SourceLocation IrLoc) {
    Sites.emplace(S, Walked::Site{irLine(IrLoc), InVirtualBaseInit});
  }

  /// Writes a call's fact at Loc, and records it for the comparison with the IR at IrLoc.
  void emit(SourceLocation Loc, SourceLocation IrLoc, const char *Kind, const FunctionDecl *F,
            const std::string &Enums, const std::string &Receiver = "-") {
    std::string Usr = usrOf(F);
    std::string Name = nameOf(F, P);
    row(Loc, Kind, Name, typeOf(F->getType(), P), Usr, Enums, Receiver);
    static const std::set<std::string> Called = {"direct", "construct", "destruct", "new", "delete"};
    if (!Called.count(Kind))
      return;
    std::string Line = irLine(IrLoc);
    W.Calls.emplace(std::make_pair(Line, Usr), Name + " (at " + Line + ")");
    W.Callees.emplace(Usr, F);
  }

  void emitIndirect(SourceLocation Loc, QualType CalleeType, const std::string &Enums) {
    QualType T = CalleeType;
    if (const auto *Ptr = T->getAs<PointerType>())
      T = Ptr->getPointeeType();
    else if (const auto *MemPtr = T->getAs<MemberPointerType>())
      T = MemPtr->getPointeeType();
    row(Loc, "indirect", "-", typeOf(T, P), "-", Enums, "-");
  }

  /// Writes a call's fact, and answers where the call is.
  std::string row(SourceLocation Loc, const char *Kind, const std::string &Name, const std::string &Signature,
                  const std::string &Usr, const std::string &Enums, const std::string &Receiver) {
    SourceLocation At = UseOverride ? *UseOverride : Loc;
    if (At.isInvalid() && !Enclosing.empty())
      At = Enclosing.back();
    if (At.isInvalid())
      fail(std::string("no location for a ") + Kind + " call of " + Name + " in " + RoutineUsr);
    std::string Line = L.line(At);
    Out.insert("call\t" + RoutineUsr + "\t" + Line + "\t" + L.outermostMacro(At) + "\t" + Kind + "\t" + Name + "\t" +
               Signature + "\t" + Usr + "\t" + Enums + "\t" + Receiver);
    return Line;
  }

  ASTContext &Context;
  PrintingPolicy P;
  const Locations &L;
  std::string RoutineUsr;
  Walked &W;
  std::set<std::string> &Out;
  std::optional<SourceLocation> UseOverride;
  std::optional<SourceLocation> IrOverride;
  // The start of each statement enclosing the one visited that has a location, innermost last.
  std::vector<SourceLocation> Enclosing;
  // Whether clang emits code for the statement visited next.
  bool Reachable = true;
  // For each loop or switch around the statement visited, whether a break it reaches leaves it.
  std::vector<bool> Broke;
  // Where the destructor of a local of each scope around the statement visited runs: at the end
  // of a block, or at the start of the statement whose condition or initializer declares it.
  std::vector<SourceLocation> Scopes;
  // Where the full-expressions around the expression visited start.
  std::vector<SourceLocation> FullExpressions;
  bool InVirtualBaseInit = false;
};

struct Request {
  std::string Key;
  std::string Path;
  std::string Symbol;
};

/// Finds the definitions of the requested routines, and collects their calls.
class RoutineFinder : public RecursiveASTVisitor<RoutineFinder> {
public:
  RoutineFinder(ASTContext &Context, const Locations &L, const std::multimap<std::string, Request> &Requests,
                std::map<std::string, Walked> &Records, std::set<std::string> &Out)
      : Context(Context), P(policy(Context)), L(L), Requests(Requests), Records(Records), Out(Out) {}

  bool shouldVisitTemplateInstantiations() const { return true; }
  bool shouldVisitImplicitCode() const { return false; }

  bool VisitFunctionDecl(FunctionDecl *F) {
    if (!F->doesThisDeclarationHaveABody() || F->isImplicit())
      return true;
    const FunctionDecl *Pattern = F->getTemplateInstantiationPattern();
    const FunctionDecl *Written = Pattern ? Pattern : F;
    auto Range = Requests.equal_range(Written->getNameAsString());
    if (Range.first == Range.second)
      return true;
    std::string Path = L.fileOf(Written->getLocation());
    for (auto It = Range.first; It != Range.second; ++It) {
      if (It->second.Path != Path || !names(It->second.Symbol, Written))
        continue;
      if (F->isDependentContext()) {
        Out.insert("template\t" + It->second.Key);
        continue;
      }
      std::string Usr = usrOf(F);
      Out.insert("routine\t" + It->second.Key + "\t" + Usr + "\t" + nameOf(F, P) + "\t" + typeOf(F->getType(), P) +
                 "\t" + L.line(F->getLocation()));
      auto [Record, New] = Records.try_emplace(Usr);
      if (!New)
        continue;
      Record->second.Routine = F;
      Record->second.Usr = Usr;
      CallCollector(Context, L, Record->second, Out).collect(F);
    }
    return true;
  }

private:
  ASTContext &Context;
  PrintingPolicy P;
  const Locations &L;
  const std::multimap<std::string, Request> &Requests;
  std::map<std::string, Walked> &Records;
  std::set<std::string> &Out;
};

/// The names of the C++ runtime's functions that clang's IR calls to throw and catch, to
/// initialize static locals, to destroy them at the process's or the thread's exit, and to end the
/// process on an exception that must not escape, by what in the source makes a routine call them.
enum class RuntimeCause { Throw, Rethrow, Catch, StaticInit, ExitRegistration, ThreadExitRegistration, Terminate };

std::optional<RuntimeCause> runtimeCause(llvm::StringRef Name) {
  static const std::map<std::string, RuntimeCause> Causes = {
      {"__cxa_allocate_exception", RuntimeCause::Throw},
      {"__cxa_throw", RuntimeCause::Throw},
      {"__cxa_free_exception", RuntimeCause::Throw},
      {"__cxa_rethrow", RuntimeCause::Rethrow},
      {"__cxa_begin_catch", RuntimeCause::Catch},
      {"__cxa_end_catch", RuntimeCause::Catch},
      {"__cxa_get_exception_ptr", RuntimeCause::Catch},
      {"__cxa_guard_acquire", RuntimeCause::StaticInit},
      {"__cxa_guard_release", RuntimeCause::StaticInit},
      {"__cxa_guard_abort", RuntimeCause::StaticInit},
      {"__cxa_atexit", RuntimeCause::ExitRegistration},
      {"__cxa_thread_atexit", RuntimeCause::ThreadExitRegistration},
      {"__clang_call_terminate", RuntimeCause::Terminate},
  };
  auto Found = Causes.find(Name.str());
  if (Found == Causes.end())
    return std::nullopt;
  return Found->second;
}

/// The intrinsic clang lowers a call of a library builtin to, where it may.
std::optional<std::string> loweredTo(const FunctionDecl *F) {
  switch (F->getBuiltinID()) {
  case Builtin::BImemcpy:
    return "llvm.memcpy";
  case Builtin::BImemmove:
    return "llvm.memmove";
  case Builtin::BImemset:
    return "llvm.memset";
  default:
    return std::nullopt;
  }
}

/// Holds what the walk found each requested routine in the unit runs to what clang's IR for the
/// unit calls from the routine's definition, at no optimisation, and fails on any difference. Each
/// call is compared by the file and line clang's line table gives it, and by its callee.
/// The IR of a routine is every function clang emits for its definition, but the deleting
/// destructor, which a delete-expression's call reaches, with every function defined inside it.
/// Writes `checked USR` for each routine whose IR agrees.
class IrChecker : public ASTConsumer {
public:
  IrChecker(CodeGenerator &Gen, const Trees &T, const std::map<std::string, Walked> &Records,
            std::set<std::string> &Out)
      : Gen(Gen), T(T), Records(Records), Out(Out) {}

  void HandleTranslationUnit(ASTContext &) override {
    llvm::Module *M = Gen.GetModule();
    if (!M)
      fail("clang emitted no IR for the unit");
    std::map<std::string, std::vector<const llvm::Function *>> ByUsr;
    for (const llvm::Function &Fn : *M) {
      if (Fn.isDeclaration())
        continue;
      if (const auto *F = dyn_cast_or_null<FunctionDecl>(Gen.GetDeclForMangledName(Fn.getName())))
        if (!isDeleting(F, Fn))
          ByUsr[usrOf(F)].push_back(&Fn);
    }
    for (const auto &[Usr, W] : Records)
      check(W, ByUsr);
    if (!Refusals.empty()) {
      std::string All;
      for (const std::string &R : Refusals)
        All += (All.empty() ? "" : "\nnative-call-graph: ") + R;
      fail(All);
    }
  }

private:
  bool isDeleting(const FunctionDecl *F, const llvm::Function &Fn) {
    const auto *Dtor = dyn_cast<CXXDestructorDecl>(F);
    return Dtor && Dtor->isVirtual() && Fn.getName() == Gen.GetMangledName(GlobalDecl(Dtor, Dtor_Deleting));
  }

  /// Whether an IR function is the base-object variant of a constructor, which does not construct
  /// the virtual bases.
  bool isBaseVariant(const FunctionDecl *F, const llvm::Function &Fn) {
    const auto *Ctor = dyn_cast<CXXConstructorDecl>(F);
    return Ctor && Ctor->getParent()->getNumVBases() && Fn.getName() == Gen.GetMangledName(GlobalDecl(Ctor, Ctor_Base));
  }

  /// What a set of IR functions calls, each by line.
  struct Tally {
    std::map<std::pair<std::string, std::string>, std::string> Calls; // (line, USR) -> name
    std::set<std::pair<std::string, std::string>> Intrinsics;          // (line, family, as llvm.memcpy)
    std::set<RuntimeCause> Runtime;
    std::map<std::string, size_t> Indirect;
    std::map<std::string, size_t> Asm;
  };

  static std::map<std::string, size_t> expected(const std::map<const Stmt *, Walked::Site> &Sites, bool Base) {
    std::map<std::string, size_t> Counts;
    for (const auto &[S, Site] : Sites)
      if (!(Base && Site.CompleteOnly))
        ++Counts[Site.Line];
    return Counts;
  }

  static std::string counts(const std::map<std::string, size_t> &Counts) {
    std::string S;
    for (const auto &[Line, N] : Counts)
      S += (S.empty() ? "" : ", ") + std::to_string(N) + " at " + Line;
    return S.empty() ? "none" : S;
  }

  void check(const Walked &W, const std::map<std::string, std::vector<const llvm::Function *>> &ByUsr) {
    size_t Before = Refusals.size();
    std::string Routine = W.Routine->getQualifiedNameAsString();
    auto Variants = ByUsr.find(W.Usr);
    if (Variants == ByUsr.end()) {
      refuse("clang emitted no IR for the definition of " + Routine);
      return;
    }
    std::set<std::string> Own = {W.Usr};
    std::vector<const llvm::Function *> Nested;
    for (const FunctionDecl *F : W.Nested) {
      std::string Usr = usrOf(F);
      Own.insert(Usr);
      if (auto Found = ByUsr.find(Usr); Found != ByUsr.end())
        Nested.insert(Nested.end(), Found->second.begin(), Found->second.end());
    }
    std::set<std::pair<std::string, std::string>> Called;
    std::set<std::pair<std::string, std::string>> Intrinsics;
    std::set<RuntimeCause> Runtime;
    for (const llvm::Function *Variant : Variants->second) {
      std::string Which = Routine + " (" + Variant->getName().str() + ")";
      bool Base = isBaseVariant(W.Routine, *Variant);
      std::set<const llvm::Function *> Reached(Nested.begin(), Nested.end());
      Reached.insert(Variant);
      Tally Got = tally(Reached, Own, ByUsr, Routine);
      if (std::map<std::string, size_t> Want = expected(W.Indirect, Base); Got.Indirect != Want)
        refuse("the IR of " + Which + " calls through a pointer " + counts(Got.Indirect) + ", and the walk " +
             counts(Want));
      if (std::map<std::string, size_t> Want = expected(W.Asm, Base); Got.Asm != Want)
        refuse("the IR of " + Which + " has inline assembly " + counts(Got.Asm) + ", and the walk " + counts(Want));
      std::vector<std::string> Extra;
      for (const auto &[Key, Name] : Got.Calls)
        if (!W.Calls.count(Key) && !Own.count(Key.second))
          Extra.push_back(Name + " (at " + Key.first + ")");
      if (!Extra.empty())
        refuse("the IR of " + Routine + " calls what the walk does not name: " + join(Extra));
      for (const auto &[Key, Name] : Got.Calls)
        Called.insert(Key);
      Intrinsics.insert(Got.Intrinsics.begin(), Got.Intrinsics.end());
      Runtime.insert(Got.Runtime.begin(), Got.Runtime.end());
    }
    std::vector<std::string> Missing;
    for (const auto &[Key, Name] : W.Calls) {
      if (Called.count(Key))
        continue;
      if (auto Callee = W.Callees.find(Key.second); Callee != W.Callees.end())
        if (std::optional<std::string> Lowered = loweredTo(Callee->second);
            Lowered && Intrinsics.count({Key.first, *Lowered}))
          continue;
      Missing.push_back(Name);
    }
    if (!Missing.empty())
      refuse("the walk of " + Routine + " names what its IR does not call: " + join(Missing));
    auto Expect = [&](bool Walked, RuntimeCause Cause, const char *What) {
      if (Walked != (Runtime.count(Cause) != 0))
        refuse("the walk of " + Routine + (Walked ? " finds " : " does not find ") + What + ", and its IR " +
             (Walked ? "does not call" : "calls") + " the runtime for it");
    };
    Expect(W.Throws, RuntimeCause::Throw, "a throw");
    Expect(W.Rethrows, RuntimeCause::Rethrow, "a rethrow");
    Expect(W.Catches, RuntimeCause::Catch, "a catch");
    Expect(W.GuardsStatic, RuntimeCause::StaticInit, "a static local initialized at run time");
    Expect(W.RegistersExit, RuntimeCause::ExitRegistration, "a static local destroyed at exit");
    Expect(W.RegistersThreadExit, RuntimeCause::ThreadExitRegistration, "a static thread_local destroyed at exit");
    if (Refusals.size() == Before)
      Out.insert("checked\t" + W.Usr);
  }

  /// The file and line clang's line table gives an instruction, as the walk writes one.
  std::string lineOf(const llvm::Instruction &I) {
    const llvm::DILocation *Loc = I.getDebugLoc().get();
    if (!Loc)
      return "(no line)";
    llvm::SmallString<256> Path(Loc->getFilename());
    if (!llvm::sys::path::is_absolute(Path)) {
      llvm::SmallString<256> Full(Loc->getDirectory());
      llvm::sys::path::append(Full, Path);
      Path = Full;
    }
    auto Found = Files.find(std::string(Path));
    if (Found == Files.end())
      Found = Files.emplace(std::string(Path), T.relative(Path)).first;
    return Found->second + ":" + std::to_string(Loc->getLine());
  }

  Tally tally(std::set<const llvm::Function *> Reached, const std::set<std::string> &Own,
              const std::map<std::string, std::vector<const llvm::Function *>> &ByUsr, const std::string &Routine) {
    Tally Got;
    std::vector<const llvm::Function *> Work(Reached.begin(), Reached.end());
    while (!Work.empty()) {
      const llvm::Function *Fn = Work.back();
      Work.pop_back();
      for (const llvm::BasicBlock &Block : *Fn)
        for (const llvm::Instruction &I : Block) {
          const auto *Call = dyn_cast<llvm::CallBase>(&I);
          if (!Call)
            continue;
          if (Call->isInlineAsm()) {
            ++Got.Asm[lineOf(I)];
            continue;
          }
          const auto *Target = dyn_cast<llvm::GlobalValue>(Call->getCalledOperand()->stripPointerCasts());
          if (!Target) {
            ++Got.Indirect[lineOf(I)];
            continue;
          }
          llvm::StringRef Name = Target->getName();
          if (const auto *Callee = dyn_cast<llvm::Function>(Target); Callee && Callee->isIntrinsic()) {
            Got.Intrinsics.insert({lineOf(I), Name.substr(0, Name.find('.', 5)).str()});
            continue;
          }
          if (const auto *F = dyn_cast_or_null<FunctionDecl>(Gen.GetDeclForMangledName(Name))) {
            std::string Usr = usrOf(F);
            Got.Calls.emplace(std::make_pair(lineOf(I), Usr), F->getQualifiedNameAsString());
            // A call into another of the routine's own functions continues the routine.
            if (Own.count(Usr))
              if (auto Found = ByUsr.find(Usr); Found != ByUsr.end())
                for (const llvm::Function *Next : Found->second)
                  if (Reached.insert(Next).second)
                    Work.push_back(Next);
            continue;
          }
          // Itanium's wrapper of a thread_local variable, _ZTW and the variable's mangled name
          // without its _Z, initializes the variable on its thread's first use. A variable of the
          // global namespace is named without mangling, which the wrapper spells as its length
          // and its name.
          if (Name.starts_with("_ZTW"))
            if (const auto *V = dyn_cast_or_null<VarDecl>(Gen.GetDeclForMangledName(wrapped(Name.substr(4))))) {
              Got.Calls.emplace(std::make_pair(lineOf(I), "tls:" + usrOf(V)), V->getQualifiedNameAsString());
              continue;
            }
          if (std::optional<RuntimeCause> Cause = runtimeCause(Name)) {
            Got.Runtime.insert(*Cause);
            continue;
          }
          refuse("the IR of " + Routine + " calls " + llvm::demangle(Name.str()) +
               ", which is neither a declaration of the unit's nor a function of the C++ runtime's this knows");
        }
    }
    return Got;
  }

  /// The mangled name of the variable a thread_local wrapper's name ends with.
  static std::string wrapped(llvm::StringRef Encoding) {
    size_t Digits = 0;
    while (Digits < Encoding.size() && llvm::isDigit(Encoding[Digits]))
      ++Digits;
    size_t Length = 0;
    if (Digits && !Encoding.substr(0, Digits).getAsInteger(10, Length) && Digits + Length == Encoding.size())
      return Encoding.substr(Digits).str();
    return ("_Z" + Encoding).str();
  }

  static std::string join(const std::vector<std::string> &Names) {
    std::string S;
    for (const std::string &N : Names)
      S += (S.empty() ? "" : ", ") + N;
    return S;
  }

  /// Records a difference between the walk and the IR; the unit fails on all of them once every
  /// routine is compared.
  void refuse(const std::string &Why) { Refusals.push_back(Why); }

  CodeGenerator &Gen;
  const Trees &T;
  std::vector<std::string> Refusals;
  const std::map<std::string, Walked> &Records;
  std::set<std::string> &Out;
  std::map<std::string, std::string> Files;
};

/// A virtual call to resolve: the receiver's static class, the method the call names, and whether
/// the call is a delete-expression's, which runs the deleting destructor.
struct Need {
  std::string Receiver;
  std::string Method;
  bool Deleting;
  bool operator<(const Need &O) const {
    return std::tie(Receiver, Method, Deleting) < std::tie(O.Receiver, O.Method, O.Deleting);
  }
};

/// Names, for each complete class that is or derives from a needed receiver class, the function
/// each needed call reaches in an object of that class, read from the slot of the class's vtable
/// that the call loads: one for each subobject of the receiver's class. An abstract class is the
/// dynamic class of an object while it is under construction or destruction, so its functions
/// count too, but for a pure one, whose call then aborts.
class OverriderFinder : public RecursiveASTVisitor<OverriderFinder> {
public:
  OverriderFinder(ASTContext &Context, const std::set<Need> &Needs, std::set<std::string> &Out)
      : Context(Context), P(policy(Context)), Needs(Needs), Out(Out) {}

  bool shouldVisitTemplateInstantiations() const { return true; }
  bool shouldVisitImplicitCode() const { return true; }

  bool VisitCXXRecordDecl(CXXRecordDecl *RD) {
    if (!RD->isThisDeclarationADefinition() || !RD->isCompleteDefinition() || RD->isDependentContext() ||
        !RD->isPolymorphic())
      return true;
    std::map<std::string, const CXXRecordDecl *> Classes; // USR -> RD or a base of it
    Classes.emplace(usrOf(RD), RD);
    RD->forallBases([&](const CXXRecordDecl *Base) {
      Classes.emplace(usrOf(Base->getDefinition()), Base->getDefinition());
      return true;
    });
    for (const Need &N : Needs) {
      auto Receiver = Classes.find(N.Receiver);
      if (Receiver == Classes.end())
        continue;
      const CXXMethodDecl *Method = methodOf(Receiver->second, N.Method);
      for (CharUnits Offset : pathOffsets(RD, RD, Receiver->second, CharUnits::Zero())) {
        const CXXMethodDecl *Final = slot(RD, Receiver->second, Offset, Method, N.Deleting);
        // A pure method's slot is never called: a call of it while its class is under
        // construction aborts.
        if (!Final || Final->isPureVirtual())
          continue;
        std::string Head = "\t" + N.Receiver + "\t" + N.Method + "\t" + (N.Deleting ? "delete" : "call") + "\t" + usrOf(RD);
        Out.insert("overrider" + Head + "\t" + nameOf(Final, P) + "\t" + typeOf(Final->getType(), P) + "\t" +
                   usrOf(Final));
        if (N.Deleting)
          if (const FunctionDecl *Delete = cast<CXXDestructorDecl>(Final)->getOperatorDelete())
            Out.insert("overrider-delete" + Head + "\t" + nameOf(Delete, P) + "\t" + typeOf(Delete->getType(), P) +
                       "\t" + usrOf(Delete));
      }
    }
    return true;
  }

private:
  /// The method of Class or a base of it that has the USR.
  const CXXMethodDecl *methodOf(const CXXRecordDecl *Class, const std::string &Usr) {
    const CXXMethodDecl *Found = nullptr;
    auto Search = [&](const CXXRecordDecl *C) {
      for (const CXXMethodDecl *M : C->methods())
        if (!Found && M->isVirtual() && usrOf(M) == Usr)
          Found = M;
      return true;
    };
    Search(Class);
    Class->forallBases(Search);
    if (!Found)
      fail("no virtual method " + Usr + " in " + Class->getQualifiedNameAsString() + " or its bases");
    return Found;
  }

  /// The offset in Complete of each subobject of type Base inside the subobject at Start of type
  /// Derived, whose virtual bases are Complete's.
  std::set<CharUnits> pathOffsets(const CXXRecordDecl *Complete, const CXXRecordDecl *Derived,
                                  const CXXRecordDecl *Base, CharUnits Start) {
    if (Derived->getCanonicalDecl() == Base->getCanonicalDecl())
      return {Start};
    CXXBasePaths Paths(/*FindAmbiguities=*/true, /*RecordPaths=*/true, /*DetectVirtual=*/false);
    if (!Derived->isDerivedFrom(Base, Paths))
      fail(Derived->getQualifiedNameAsString() + " does not derive from " + Base->getQualifiedNameAsString());
    std::set<CharUnits> Offsets;
    for (const CXXBasePath &Path : Paths) {
      CharUnits Offset = Start;
      for (const CXXBasePathElement &Step : Path) {
        const CXXRecordDecl *Next = Step.Base->getType()->getAsCXXRecordDecl();
        if (Step.Base->isVirtual())
          Offset = Context.getASTRecordLayout(Complete).getVBaseClassOffset(Next);
        else
          Offset += Context.getASTRecordLayout(Step.Class).getBaseClassOffset(Next);
      }
      Offsets.insert(Offset);
    }
    return Offsets;
  }

  /// The function in the slot of Complete's vtable that a call of Method through the subobject at
  /// Offset of type Receiver loads.
  const CXXMethodDecl *slot(const CXXRecordDecl *Complete, const CXXRecordDecl *Receiver, CharUnits Offset,
                            const CXXMethodDecl *Method, bool Deleting) {
    std::set<CharUnits> Declaring = pathOffsets(Complete, Receiver, Method->getParent(), Offset);
    if (Declaring.size() != 1)
      fail("a call of " + Method->getQualifiedNameAsString() + " through " + Receiver->getQualifiedNameAsString() +
           " reaches more than one subobject of " + Method->getParent()->getQualifiedNameAsString());
    GlobalDecl Called(Method);
    if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(Method))
      Called = GlobalDecl(Dtor, Deleting ? Dtor_Deleting : Dtor_Complete);
    auto *VTables = cast<ItaniumVTableContext>(Context.getVTableContext());
    const VTableLayout &Layout = VTables->getVTableLayout(Complete);
    auto Point = Layout.getAddressPoints().find(BaseSubobject(Method->getParent(), *Declaring.begin()));
    if (Point == Layout.getAddressPoints().end())
      fail("no vtable address point for " + Method->getParent()->getQualifiedNameAsString() + " in " +
           Complete->getQualifiedNameAsString());
    size_t Index = Layout.getVTableOffset(Point->second.VTableIndex) + Point->second.AddressPointIndex +
                   VTables->getMethodVTableIndex(Called);
    const VTableComponent &Component = Layout.vtable_components()[Index];
    // A slot clang marks unused is one no call loads.
    if (Component.getKind() == VTableComponent::CK_UnusedFunctionPointer)
      return nullptr;
    if (!Component.isUsedFunctionPointerKind())
      fail("the vtable slot of " + Method->getQualifiedNameAsString() + " in " + Complete->getQualifiedNameAsString() +
           " holds no function");
    return Component.getFunctionDecl();
  }

  ASTContext &Context;
  PrintingPolicy P;
  const std::set<Need> &Needs;
  std::set<std::string> &Out;
};

enum class Mode { Routines, Overriders };

struct Inputs {
  Mode M;
  Trees T;
  std::multimap<std::string, Request> Requests;      // by the last component of the symbol
  std::set<Need> Needs;
};

/// Walks the requested routines, or finds the overriders; for routines, also schedules clang's IR
/// for each routine's definition and the functions defined inside it, so that the code generator
/// that follows it emits them even where nothing in the unit uses them.
class Walker : public ASTConsumer {
public:
  Walker(CompilerInstance &CI, const Inputs &In, CodeGenerator *Gen, std::map<std::string, Walked> &Records,
         std::set<std::string> &Out)
      : CI(CI), In(In), Gen(Gen), Records(Records), Out(Out) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    if (CI.getDiagnostics().hasErrorOccurred())
      return;
    Locations L(Context.getSourceManager(), Context.getLangOpts(), CI.getPreprocessor(), In.T);
    if (In.M == Mode::Overriders) {
      OverriderFinder(Context, In.Needs, Out).TraverseDecl(Context.getTranslationUnitDecl());
      return;
    }
    RoutineFinder(Context, L, In.Requests, Records, Out).TraverseDecl(Context.getTranslationUnitDecl());
    for (const auto &[Usr, W] : Records) {
      emit(W.Routine);
      for (const FunctionDecl *F : W.Nested)
        emit(F);
    }
  }

private:
  void emit(const FunctionDecl *F) {
    if (!F->doesThisDeclarationHaveABody() || F->isDependentContext())
      return;
    if (const auto *Ctor = dyn_cast<CXXConstructorDecl>(F)) {
      Gen->GetAddrOfGlobal(GlobalDecl(Ctor, Ctor_Complete), false);
      Gen->GetAddrOfGlobal(GlobalDecl(Ctor, Ctor_Base), false);
    } else if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(F)) {
      Gen->GetAddrOfGlobal(GlobalDecl(Dtor, Dtor_Complete), false);
      Gen->GetAddrOfGlobal(GlobalDecl(Dtor, Dtor_Base), false);
    } else
      Gen->GetAddrOfGlobal(GlobalDecl(F), false);
  }

  CompilerInstance &CI;
  const Inputs &In;
  CodeGenerator *Gen;
  std::map<std::string, Walked> &Records;
  std::set<std::string> &Out;
};

class Action : public ASTFrontendAction {
public:
  Action(const Inputs &In, std::set<std::string> &Out) : In(In), Out(Out) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef InFile) override {
    if (In.M == Mode::Overriders)
      return std::make_unique<Walker>(CI, In, nullptr, Records, Out);
    // The IR as clang first emits it, before any optimisation, with a line table, as
    // -gline-tables-only has it.
    CI.getCodeGenOpts().OptimizationLevel = 0;
    CI.getCodeGenOpts().setDebugInfo(llvm::codegenoptions::DebugLineTablesOnly);
    std::unique_ptr<CodeGenerator> Gen(CreateLLVMCodeGen(CI.getDiagnostics(), InFile, &CI.getVirtualFileSystem(),
                                                         CI.getHeaderSearchOpts(), CI.getPreprocessorOpts(),
                                                         CI.getCodeGenOpts(), LLVM));
    CodeGenerator *Generator = Gen.get();
    std::vector<std::unique_ptr<ASTConsumer>> Consumers;
    Consumers.push_back(std::make_unique<Walker>(CI, In, Generator, Records, Out));
    Consumers.push_back(std::move(Gen));
    Consumers.push_back(std::make_unique<IrChecker>(*Generator, In.T, Records, Out));
    return std::make_unique<MultiplexConsumer>(std::move(Consumers));
  }

private:
  const Inputs &In;
  std::set<std::string> &Out;
  std::map<std::string, Walked> Records;
  llvm::LLVMContext LLVM;
};

/// The command, to check syntax only, without the precompiled header (whose source it includes).
std::vector<std::string> adjust(const tooling::CompileCommand &Command, const std::string &ResourceDir) {
  std::vector<std::string> Args;
  for (size_t I = 0; I < Command.CommandLine.size(); ++I) {
    const std::string &A = Command.CommandLine[I];
    if (A == "-Xclang" && I + 3 < Command.CommandLine.size() && Command.CommandLine[I + 1] == "-include-pch" &&
        Command.CommandLine[I + 2] == "-Xclang") {
      I += 3;
      continue;
    }
    Args.push_back(A);
  }
  Args = tooling::getClangStripOutputAdjuster()(Args, Command.Filename);
  Args = tooling::getClangStripDependencyFileAdjuster()(Args, Command.Filename);
  Args = tooling::getClangSyntaxOnlyAdjuster()(Args, Command.Filename);
  Args.insert(Args.begin() + 1, {"-resource-dir", ResourceDir, "-working-directory", Command.Directory});
  return Args;
}

} // namespace

int main(int argc, char **argv) {
  if (argc < 5)
    fail("usage: native-call-graph (routines|overriders) DB INPUT ROOT [PREFIX=PATH]...");
  Inputs In;
  std::string ModeName = argv[1];
  if (ModeName == "routines")
    In.M = Mode::Routines;
  else if (ModeName == "overriders")
    In.M = Mode::Overriders;
  else
    fail("unknown mode " + ModeName);
  In.T.Prefixes.emplace_back("", realPath(argv[4]));
  for (int I = 5; I < argc; ++I) {
    std::string Arg = argv[I];
    size_t Eq = Arg.find('=');
    if (Eq == std::string::npos || Eq == 0)
      fail("expected PREFIX=PATH, not " + Arg);
    In.T.Prefixes.emplace_back(Arg.substr(0, Eq), realPath(Arg.substr(Eq + 1)));
  }
  if (In.M == Mode::Routines)
    for (auto &Row : readTsv(argv[3], 3))
      In.Requests.emplace(lastComponent(Row[2]), Request{Row[0], Row[1], Row[2]});
  else
    for (auto &Row : readTsv(argv[3], 3)) {
      if (Row[2] != "call" && Row[2] != "delete")
        fail("a need is a call or a delete, not " + Row[2]);
      In.Needs.insert(Need{Row[0], Row[1], Row[2] == "delete"});
    }

  const std::string ResourceDir = CLANG_RESOURCE_DIR;
  std::string Error;
  auto Db = tooling::JSONCompilationDatabase::loadFromFile(argv[2], Error, tooling::JSONCommandLineSyntax::AutoDetect);
  if (!Db)
    fail(Error);
  std::vector<tooling::CompileCommand> Commands = Db->getAllCompileCommands();
  if (Commands.empty())
    fail(std::string("no commands in ") + argv[2]);

  std::set<std::string> Out;
  for (const tooling::CompileCommand &Command : Commands) {
    llvm::IntrusiveRefCntPtr<FileManager> Files(new FileManager(FileSystemOptions()));
    tooling::ToolInvocation Invocation(adjust(Command, ResourceDir), std::make_unique<Action>(In, Out), Files.get());
    if (!Invocation.run())
      fail("errors in " + Command.Filename);
  }
  for (const std::string &Line : Out) {
    if (Line.find('\n') != std::string::npos)
      fail("a fact with a newline in it: " + Line);
    llvm::outs() << Line << "\n";
  }
  return 0;
}
