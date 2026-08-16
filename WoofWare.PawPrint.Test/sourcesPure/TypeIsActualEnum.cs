using System;

enum GuestIntEnum { A, B = 5 }

enum GuestByteEnum : byte { X, Y }

struct GuestStruct { public int Field; }

class GuestClass { }

class EnumConstrained<T> where T : Enum { }

class Unconstrained<T> { }

class ClosedBase { }

class DerivedFromClosedBase<T> : ClosedBase { }

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    // `RuntimeType.IsActualEnum` is internal, so it is observed through the property it gates:
    // `RuntimeType.GetEnumUnderlyingType` is `if (!IsActualEnum) ThrowMustBeEnum(); ...`, and
    // `ThrowMustBeEnum` raises exactly `ArgumentException`.
    static bool IsActualEnum(Type t)
    {
        try
        {
            t.GetEnumUnderlyingType();
            return true;
        }
        catch (ArgumentException)
        {
            return false;
        }
    }

    static bool ThrowsArgumentException(Action f)
    {
        try
        {
            f();
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }

    static unsafe int Main(string[] args)
    {
        // The truth table. `IsActualEnum` is "is this type's immediate base `System.Enum`",
        // asked of a MethodTable, so every non-enum shape below has to answer false for its own
        // reason rather than by accident.
        Check(IsActualEnum(typeof(GuestIntEnum)));
        Check(IsActualEnum(typeof(GuestByteEnum)));
        // A corelib enum: the base-type comparison crosses assemblies.
        Check(IsActualEnum(typeof(DayOfWeek)));
        // Underlying integer of an enum: a primitive, whose base is `System.ValueType`.
        Check(!IsActualEnum(typeof(int)));
        Check(!IsActualEnum(typeof(GuestStruct)));
        Check(!IsActualEnum(typeof(GuestClass)));
        // `System.Enum` itself derives from `System.ValueType`, not from itself.
        Check(!IsActualEnum(typeof(Enum)));
        Check(!IsActualEnum(typeof(ValueType)));
        Check(!IsActualEnum(typeof(object)));
        // An array of enums is a MethodTable whose base is `System.Array`.
        Check(!IsActualEnum(typeof(GuestIntEnum[])));
        // `Nullable<TEnum>` is a value type in its own right; its base is `System.ValueType`.
        Check(!IsActualEnum(typeof(GuestIntEnum?)));
        // A pointer has no MethodTable at all, so the answer comes from the `IsTypeDesc`
        // short-circuit rather than from any base-type read.
        Check(!IsActualEnum(typeof(int*)));

        // The distinction `IsActualEnum` exists to draw: a generic parameter constrained to
        // `Enum` reports `IsEnum` true (the constraint walk finds `System.Enum` as its base)
        // but is not itself an enum, so `IsActualEnum` is false and the enum reflection surface
        // refuses it. An implementation that answered `IsEnum` would fail here.
        Type constrained = typeof(EnumConstrained<>).GetGenericArguments()[0];
        Check(constrained.IsEnum);
        Check(!IsActualEnum(constrained));
        Type unconstrained = typeof(Unconstrained<>).GetGenericArguments()[0];
        Check(!unconstrained.IsEnum);
        Check(!IsActualEnum(unconstrained));

        // An open generic *definition* is a MethodTable, not a TypeDesc, so it does not take the
        // short-circuit above: the base-type read really happens, down the
        // `OpenGenericTypeDefinition` arm of the `ParentMethodTable` projection rather than the
        // `Closed` one every other case here uses. Both parents that arm can name are covered —
        // `System.Object`, and a closed non-generic base. (A base that still mentions the type
        // parameter is the shape `resolveBaseRuntimeTypeHandleTarget` refuses; it is parked in
        // `EnumQueriesOpenGenericSharedParent.cs`, and `IsEnum` reaches that refusal too.)
        Check(!typeof(Unconstrained<>).IsEnum);
        Check(!IsActualEnum(typeof(Unconstrained<>)));
        Check(!typeof(DerivedFromClosedBase<>).IsEnum);
        Check(!IsActualEnum(typeof(DerivedFromClosedBase<>)));

        // The reflection surface `IsActualEnum` gates, in its allowing direction.
        Check(typeof(GuestIntEnum).GetEnumUnderlyingType() == typeof(int));
        Check(typeof(GuestByteEnum).GetEnumUnderlyingType() == typeof(byte));
        Check(Enum.GetUnderlyingType(typeof(DayOfWeek)) == typeof(int));
        string[] names = typeof(GuestIntEnum).GetEnumNames();
        Check(names.Length == 2);
        Check(names[0] == "A");
        Check(names[1] == "B");
        Check(typeof(GuestIntEnum).IsEnumDefined(GuestIntEnum.B));
        Check(!typeof(GuestIntEnum).IsEnumDefined((GuestIntEnum)7));
        Check(Type.GetTypeCode(typeof(GuestByteEnum)) == TypeCode.Byte);
        Check(Type.GetTypeCode(typeof(GuestIntEnum)) == TypeCode.Int32);
        Check(Enum.GetName(typeof(GuestIntEnum), GuestIntEnum.B) == "B");

        // And in its refusing direction, at call sites other than `GetEnumUnderlyingType`.
        Check(ThrowsArgumentException(() => { var x = typeof(int).GetEnumNames(); }));
        Check(ThrowsArgumentException(() => { var x = typeof(GuestClass).IsEnumDefined(1); }));
        Check(ThrowsArgumentException(() => { var x = Enum.GetUnderlyingType(typeof(Enum)); }));
        Check(ThrowsArgumentException(() => { var x = typeof(GuestIntEnum[]).GetEnumValuesAsUnderlyingType(); }));

        // A non-enum reaches `Type.GetTypeCode` through the same `IsActualEnum` test, which must
        // decline to look for an underlying type.
        Check(Type.GetTypeCode(typeof(int)) == TypeCode.Int32);
        Check(Type.GetTypeCode(typeof(GuestClass)) == TypeCode.Object);

        return firstFailure;
    }
}
