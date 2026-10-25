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
// where KIND is direct, dispatch (a virtual call, to be resolved by generate.py against the
// receiver's class), indirect, construct, destruct, new, delete, throw or asm.
//
// `overriders` reads NEEDS (lines of a receiver class's USR and a virtual method's USR) and writes
//   overrider RECEIVER-USR METHOD-USR CLASS-USR NAME SIGNATURE USR
// for each class the unit completes that is not abstract and is or derives from the receiver
// class, naming the final overrider of the method in it.

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/ArgumentsAdjusters.h"
#include "clang/Tooling/JSONCompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
#include <fstream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
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

/// Every call one routine's definition makes.
class CallCollector : public RecursiveASTVisitor<CallCollector> {
public:
  CallCollector(ASTContext &Context, const Locations &L, std::string RoutineUsr, std::set<std::string> &Out)
      : Context(Context), P(policy(Context)), L(L), RoutineUsr(std::move(RoutineUsr)), Out(Out) {}

  bool shouldVisitImplicitCode() const { return true; }
  bool shouldVisitTemplateInstantiations() const { return true; }

  void collect(const FunctionDecl *F) {
    if (const auto *Ctor = dyn_cast<CXXConstructorDecl>(F)) {
      for (const CXXCtorInitializer *Init : Ctor->inits())
        TraverseStmt(Init->getInit());
      // A constructor that throws destroys what it has constructed.
      destroyBasesAndMembers(Ctor->getParent(), F->getBody()->getEndLoc());
    }
    for (const ParmVarDecl *Param : F->parameters()) {
      const CXXRecordDecl *RD = Param->getType()->getAsCXXRecordDecl();
      if (RD && RD->isParamDestroyedInCallee())
        if (const CXXDestructorDecl *D = nonTrivialDestructor(Param->getType(), Context))
          emit(Param->getLocation(), "destruct", D, "-");
    }
    TraverseStmt(F->getBody());
    if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(F))
      destroyBasesAndMembers(Dtor->getParent(), F->getBody()->getEndLoc());
  }

  // Recursion rather than RecursiveASTVisitor's queue, so that Enclosing brackets every child.
  bool TraverseStmt(Stmt *S, DataRecursionQueue * = nullptr) {
    if (!S)
      return true;
    bool Located = S->getBeginLoc().isValid();
    if (Located)
      Enclosing.push_back(S->getBeginLoc());
    bool Result = RecursiveASTVisitor::TraverseStmt(S, nullptr);
    if (Located)
      Enclosing.pop_back();
    return Result;
  }

  // Unevaluated operands and compile-time evaluation run nothing.
  bool TraverseUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *) { return true; }
  bool TraverseCXXNoexceptExpr(CXXNoexceptExpr *) { return true; }
  bool TraverseDecltypeTypeLoc(DecltypeTypeLoc, bool = false) { return true; }
  bool TraverseTypeOfExprTypeLoc(TypeOfExprTypeLoc, bool = false) { return true; }
  bool TraverseStaticAssertDecl(StaticAssertDecl *) { return true; }
  bool TraverseConstantExpr(ConstantExpr *) { return true; }
  bool TraverseCXXTypeidExpr(CXXTypeidExpr *E) {
    return E->isPotentiallyEvaluated() ? RecursiveASTVisitor::TraverseCXXTypeidExpr(E) : true;
  }
  bool TraverseIfStmt(IfStmt *S) {
    if (!S->isConstexpr())
      return RecursiveASTVisitor::TraverseIfStmt(S);
    std::optional<Stmt *> Taken = S->getNondiscardedCase(Context);
    if (!Taken)
      fail("an if constexpr whose condition is value-dependent at " + L.line(S->getIfLoc()));
    return TraverseStmt(S->getInit()) && TraverseStmt(*Taken);
  }

  // A default argument or member initializer runs where it is used.
  bool TraverseCXXDefaultArgExpr(CXXDefaultArgExpr *E) { return within(E->getUsedLocation(), E->getExpr()); }
  bool TraverseCXXDefaultInitExpr(CXXDefaultInitExpr *E) { return within(E->getUsedLocation(), E->getExpr()); }

  bool VisitCallExpr(CallExpr *E) {
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
      return true;
    }
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
      const CXXRecordDecl *Class = recordOf(receiverOf(Receiver)->getType());
      if (!Class)
        fail("a virtual call whose receiver is not of a class at " + L.line(Loc));
      emit(Loc, "dispatch", F, enums(E, FirstArg), usrOf(Class->getDefinition()));
      return true;
    }
    emit(Loc, "direct", F, enums(E, FirstArg));
    return true;
  }

  bool VisitCXXConstructExpr(CXXConstructExpr *E) {
    const CXXConstructorDecl *Ctor = E->getConstructor();
    if (!Ctor->isTrivial())
      emit(E->getLocation(), "construct", Ctor, enums(E));
    return true;
  }

  bool VisitCXXInheritedCtorInitExpr(CXXInheritedCtorInitExpr *E) {
    emit(E->getLocation(), "construct", E->getConstructor(), "-");
    return true;
  }

  bool VisitCXXNewExpr(CXXNewExpr *E) {
    if (!E->getOperatorNew())
      fail("a new expression with no operator new at " + L.line(E->getBeginLoc()));
    emit(E->getBeginLoc(), "new", E->getOperatorNew(), "-");
    return true;
  }

  bool VisitCXXDeleteExpr(CXXDeleteExpr *E) {
    if (E->getDestroyedType()->isDependentType())
      fail("a delete of a dependent type at " + L.line(E->getBeginLoc()));
    if (const CXXDestructorDecl *D = nonTrivialDestructor(E->getDestroyedType(), Context)) {
      if (D->isVirtual() && !E->isArrayForm())
        emit(E->getBeginLoc(), "dispatch", D, "-", usrOf(D->getParent()->getDefinition()));
      else
        emit(E->getBeginLoc(), "destruct", D, "-");
    }
    if (!E->getOperatorDelete())
      fail("a delete expression with no operator delete at " + L.line(E->getBeginLoc()));
    emit(E->getBeginLoc(), "delete", E->getOperatorDelete(), "-");
    return true;
  }

  bool VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *E) {
    emit(E->getBeginLoc(), "destruct", E->getTemporary()->getDestructor(), "-");
    return true;
  }

  bool VisitVarDecl(VarDecl *V) {
    if (isa<ParmVarDecl>(V) || !V->hasLocalStorage())
      return true;
    if (V->getType()->isReferenceType())
      return true;
    if (const CXXDestructorDecl *D = nonTrivialDestructor(V->getType(), Context))
      emit(V->getLocation(), "destruct", D, "-");
    return true;
  }

  bool VisitCXXThrowExpr(CXXThrowExpr *E) {
    const Expr *Thrown = E->getSubExpr();
    std::string What = Thrown ? typeOf(Thrown->getType(), P) : "-";
    row(E->getThrowLoc(), "throw", What, "-", "-", "-", "-");
    return true;
  }

  bool VisitAsmStmt(AsmStmt *S) {
    row(S->getAsmLoc(), "asm", "-", "-", "-", "-", "-");
    return true;
  }

private:
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
        emit(Loc, "destruct", D, "-");
    for (const CXXBaseSpecifier &Base : RD->vbases())
      if (const CXXDestructorDecl *D = nonTrivialDestructor(Base.getType(), Context))
        emit(Loc, "destruct", D, "-");
    if (RD->isUnion())
      return;
    for (const FieldDecl *Field : RD->fields())
      if (const CXXDestructorDecl *D = nonTrivialDestructor(Field->getType(), Context))
        emit(Loc, "destruct", D, "-");
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

  void emit(SourceLocation Loc, const char *Kind, const FunctionDecl *F, const std::string &Enums,
            const std::string &Receiver = "-") {
    row(Loc, Kind, nameOf(F, P), typeOf(F->getType(), P), usrOf(F), Enums, Receiver);
  }

  void emitIndirect(SourceLocation Loc, QualType CalleeType, const std::string &Enums) {
    QualType T = CalleeType;
    if (const auto *Ptr = T->getAs<PointerType>())
      T = Ptr->getPointeeType();
    else if (const auto *MemPtr = T->getAs<MemberPointerType>())
      T = MemPtr->getPointeeType();
    row(Loc, "indirect", "-", typeOf(T, P), "-", Enums, "-");
  }

  void row(SourceLocation Loc, const char *Kind, const std::string &Name, const std::string &Signature,
           const std::string &Usr, const std::string &Enums, const std::string &Receiver) {
    SourceLocation At = UseOverride ? *UseOverride : Loc;
    if (At.isInvalid() && !Enclosing.empty())
      At = Enclosing.back();
    if (At.isInvalid())
      fail(std::string("no location for a ") + Kind + " call of " + Name + " in " + RoutineUsr);
    Out.insert("call\t" + RoutineUsr + "\t" + L.line(At) + "\t" + L.outermostMacro(At) + "\t" + Kind + "\t" + Name +
               "\t" + Signature + "\t" + Usr + "\t" + Enums + "\t" + Receiver);
  }

  ASTContext &Context;
  PrintingPolicy P;
  const Locations &L;
  std::string RoutineUsr;
  std::set<std::string> &Out;
  std::optional<SourceLocation> UseOverride;
  // The start of each statement enclosing the one visited that has a location, innermost last.
  std::vector<SourceLocation> Enclosing;
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
                std::set<std::string> &Out)
      : Context(Context), P(policy(Context)), L(L), Requests(Requests), Out(Out) {}

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
      CallCollector(Context, L, Usr, Out).collect(F);
    }
    return true;
  }

private:
  ASTContext &Context;
  PrintingPolicy P;
  const Locations &L;
  const std::multimap<std::string, Request> &Requests;
  std::set<std::string> &Out;
};

/// Names, for each complete class that is not abstract, the final overrider of each needed method.
class OverriderFinder : public RecursiveASTVisitor<OverriderFinder> {
public:
  OverriderFinder(ASTContext &Context, const std::set<std::pair<std::string, std::string>> &Needs,
                  std::set<std::string> &Out)
      : P(policy(Context)), Needs(Needs), Out(Out) {}

  bool shouldVisitTemplateInstantiations() const { return true; }
  bool shouldVisitImplicitCode() const { return true; }

  bool VisitCXXRecordDecl(CXXRecordDecl *RD) {
    if (!RD->isThisDeclarationADefinition() || !RD->isCompleteDefinition() || RD->isDependentContext() ||
        !RD->isPolymorphic() || RD->isAbstract())
      return true;
    std::map<std::string, const CXXRecordDecl *> Classes; // USR -> RD or a base of it
    Classes.emplace(usrOf(RD), RD);
    RD->forallBases([&](const CXXRecordDecl *Base) {
      Classes.emplace(usrOf(Base->getDefinition()), Base);
      return true;
    });
    std::optional<CXXFinalOverriderMap> Overriders;
    for (const auto &[Receiver, Method] : Needs) {
      if (!Classes.count(Receiver))
        continue;
      if (!Overriders) {
        Overriders.emplace();
        RD->getFinalOverriders(*Overriders);
      }
      for (const auto &[Virtual, BySubobject] : *Overriders) {
        if (usrOf(Virtual) != Method)
          continue;
        for (const auto &[Subobject, Found] : BySubobject)
          for (const UniqueVirtualMethod &Final : Found)
            Out.insert("overrider\t" + Receiver + "\t" + Method + "\t" + usrOf(RD) + "\t" + nameOf(Final.Method, P) +
                       "\t" + typeOf(Final.Method->getType(), P) + "\t" + usrOf(Final.Method));
      }
    }
    return true;
  }

private:
  PrintingPolicy P;
  const std::set<std::pair<std::string, std::string>> &Needs;
  std::set<std::string> &Out;
};

enum class Mode { Routines, Overriders };

struct Inputs {
  Mode M;
  Trees T;
  std::multimap<std::string, Request> Requests;      // by the last component of the symbol
  std::set<std::pair<std::string, std::string>> Needs; // (receiver class USR, method USR)
};

class Consumer : public ASTConsumer {
public:
  Consumer(CompilerInstance &CI, const Inputs &In, std::set<std::string> &Out) : CI(CI), In(In), Out(Out) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    if (CI.getDiagnostics().hasErrorOccurred())
      return;
    Locations L(Context.getSourceManager(), Context.getLangOpts(), CI.getPreprocessor(), In.T);
    if (In.M == Mode::Routines)
      RoutineFinder(Context, L, In.Requests, Out).TraverseDecl(Context.getTranslationUnitDecl());
    else
      OverriderFinder(Context, In.Needs, Out).TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  CompilerInstance &CI;
  const Inputs &In;
  std::set<std::string> &Out;
};

class Action : public ASTFrontendAction {
public:
  Action(const Inputs &In, std::set<std::string> &Out) : In(In), Out(Out) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
    return std::make_unique<Consumer>(CI, In, Out);
  }

private:
  const Inputs &In;
  std::set<std::string> &Out;
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
    for (auto &Row : readTsv(argv[3], 2))
      In.Needs.emplace(Row[0], Row[1]);

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
