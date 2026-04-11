namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp

type TopLevelReferenceScenario =
    {
        DefiningAssemblyName : string
        ConsumerAssemblyName : string
        Namespace : string
        TargetName : string
        OtherTopLevelName : string
    }

type NestedReferenceScenario =
    {
        DefiningAssemblyName : string
        ConsumerAssemblyName : string
        Namespace : string
        ParentName : string
        AlternateParentName : string
        ChildName : string
        IncludeTopLevelChildCollision : bool
    }

type ForwardedTopLevelScenario =
    {
        TargetAssemblyName : string
        ForwarderAssemblyName : string
        Namespace : string
        TargetName : string
    }

type ForwardedNestedScenario =
    {
        TargetAssemblyName : string
        ForwarderAssemblyName : string
        Namespace : string
        ParentName : string
        AlternateParentName : string
        ChildName : string
        IncludeTopLevelChildCollision : bool
    }

type ResolutionScenario =
    | TopLevelReference of TopLevelReferenceScenario
    | NestedReference of NestedReferenceScenario
    | ForwardedTopLevel of ForwardedTopLevelScenario
    | ForwardedNested of ForwardedNestedScenario

[<RequireQualifiedAccess>]
module AssemblyGraphShape =

    let private namespaceNamePool : string list = [ "N" ; "M" ; "Q" ; "R" ]

    let private identifierPool : string list =
        [
            "Outer"
            "Inner"
            "Inner"
            "Box"
            "Box"
            "Node"
            "Node"
            "Holder"
            "Value"
            "Widget"
            "Thing"
            "Parent"
            "Child"
            "Alpha"
            "Beta"
            "Gamma"
            "Delta"
            "Echo"
        ]

    let rec private genDistinctFromPool<'a when 'a : equality> (count : int) (pool : 'a list) : Gen<'a list> =
        if count <= 0 then
            Gen.constant []
        else
            gen {
                let! raw = Gen.listOfLength (count * 3 + 2) (Gen.elements pool)
                let picked = raw |> List.distinct |> List.truncate count

                if List.length picked = count then
                    return picked
                else
                    return! genDistinctFromPool count pool
            }

    let topLevelTargetPath (scenario : TopLevelReferenceScenario) : TypePath =
        {
            Namespace = scenario.Namespace
            Segments =
                [
                    {
                        Name = scenario.TargetName
                        GenericArity = 0
                    }
                ]
        }

    let nestedTargetPath (scenario : NestedReferenceScenario) : TypePath =
        {
            Namespace = scenario.Namespace
            Segments =
                [
                    {
                        Name = scenario.ParentName
                        GenericArity = 0
                    }
                    {
                        Name = scenario.ChildName
                        GenericArity = 0
                    }
                ]
        }

    let forwardedTopLevelTargetPath (scenario : ForwardedTopLevelScenario) : TypePath =
        {
            Namespace = scenario.Namespace
            Segments =
                [
                    {
                        Name = scenario.TargetName
                        GenericArity = 0
                    }
                ]
        }

    let forwardedNestedTargetPath (scenario : ForwardedNestedScenario) : TypePath =
        {
            Namespace = scenario.Namespace
            Segments =
                [
                    {
                        Name = scenario.ParentName
                        GenericArity = 0
                    }
                    {
                        Name = scenario.ChildName
                        GenericArity = 0
                    }
                ]
        }

    let renderTopLevelDefiningAssembly (scenario : TopLevelReferenceScenario) : string =
        $"""
namespace {scenario.Namespace}
{{
    public class {scenario.TargetName} {{ }}
    public class {scenario.OtherTopLevelName} {{ }}
}}
"""

    let renderTopLevelConsumerAssembly (scenario : TopLevelReferenceScenario) : string =
        $"""
using {scenario.Namespace};
public class Consumer
{{
    private {scenario.TargetName} _field = new {scenario.TargetName}();
}}
"""

    let renderNestedDefiningAssembly (scenario : NestedReferenceScenario) : string =
        let topLevelCollision =
            if scenario.IncludeTopLevelChildCollision then
                $"    public class {scenario.ChildName} {{ }}\n"
            else
                ""

        $"""
namespace {scenario.Namespace}
{{
    public class {scenario.ParentName}
    {{
        public class {scenario.ChildName} {{ }}
    }}

    public class {scenario.AlternateParentName}
    {{
        public class {scenario.ChildName} {{ }}
    }}

{topLevelCollision}}}
"""

    let renderNestedConsumerAssembly (scenario : NestedReferenceScenario) : string =
        $"""
using {scenario.Namespace};
public class Consumer
{{
    private {scenario.ParentName}.{scenario.ChildName} _field = new {scenario.ParentName}.{scenario.ChildName}();
}}
"""

    let renderForwardedTopLevelTargetAssembly (scenario : ForwardedTopLevelScenario) : string =
        $"""
namespace {scenario.Namespace}
{{
    public class {scenario.TargetName} {{ }}
}}
"""

    let renderForwardedTopLevelForwarderAssembly (scenario : ForwardedTopLevelScenario) : string =
        $"""
using System.Runtime.CompilerServices;
using {scenario.Namespace};
[assembly: TypeForwardedTo(typeof({scenario.TargetName}))]
public class Placeholder {{ }}
"""

    let renderForwardedNestedTargetAssembly (scenario : ForwardedNestedScenario) : string =
        let topLevelCollision =
            if scenario.IncludeTopLevelChildCollision then
                $"    public class {scenario.ChildName} {{ }}\n"
            else
                ""

        $"""
namespace {scenario.Namespace}
{{
    public class {scenario.ParentName}
    {{
        public class {scenario.ChildName} {{ }}
    }}

    public class {scenario.AlternateParentName}
    {{
        public class {scenario.ChildName} {{ }}
    }}

{topLevelCollision}}}
"""

    let renderForwardedNestedForwarderAssembly (scenario : ForwardedNestedScenario) : string =
        $"""
using System.Runtime.CompilerServices;
using {scenario.Namespace};
[assembly: TypeForwardedTo(typeof({scenario.ParentName}))]
public class Placeholder {{ }}
"""

    let private genNamespaceName : Gen<string> = Gen.elements namespaceNamePool

    let genTopLevelReferenceScenario : Gen<TopLevelReferenceScenario> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! ``namespace`` = genNamespaceName
            let! names = genDistinctFromPool 2 identifierPool

            return
                {
                    DefiningAssemblyName = $"TypeIdentity.Property.TopLevel.Defining.{salt}"
                    ConsumerAssemblyName = $"TypeIdentity.Property.TopLevel.Consumer.{salt}"
                    Namespace = ``namespace``
                    TargetName = names.[0]
                    OtherTopLevelName = names.[1]
                }
        }

    let genNestedReferenceScenario : Gen<NestedReferenceScenario> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! ``namespace`` = genNamespaceName
            let! names = genDistinctFromPool 3 identifierPool
            let! includeTopLevelCollision = ArbMap.generate<bool> ArbMap.defaults

            return
                {
                    DefiningAssemblyName = $"TypeIdentity.Property.Nested.Defining.{salt}"
                    ConsumerAssemblyName = $"TypeIdentity.Property.Nested.Consumer.{salt}"
                    Namespace = ``namespace``
                    ParentName = names.[0]
                    AlternateParentName = names.[1]
                    ChildName = names.[2]
                    IncludeTopLevelChildCollision = includeTopLevelCollision
                }
        }

    let genForwardedTopLevelScenario : Gen<ForwardedTopLevelScenario> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! ``namespace`` = genNamespaceName
            let! name = Gen.elements identifierPool

            return
                {
                    TargetAssemblyName = $"TypeIdentity.Property.ForwardedTopLevel.Target.{salt}"
                    ForwarderAssemblyName = $"TypeIdentity.Property.ForwardedTopLevel.Forwarder.{salt}"
                    Namespace = ``namespace``
                    TargetName = name
                }
        }

    let genForwardedNestedScenario : Gen<ForwardedNestedScenario> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! ``namespace`` = genNamespaceName
            let! names = genDistinctFromPool 3 identifierPool
            let! includeTopLevelCollision = ArbMap.generate<bool> ArbMap.defaults

            return
                {
                    TargetAssemblyName = $"TypeIdentity.Property.ForwardedNested.Target.{salt}"
                    ForwarderAssemblyName = $"TypeIdentity.Property.ForwardedNested.Forwarder.{salt}"
                    Namespace = ``namespace``
                    ParentName = names.[0]
                    AlternateParentName = names.[1]
                    ChildName = names.[2]
                    IncludeTopLevelChildCollision = includeTopLevelCollision
                }
        }

    let genResolutionScenario : Gen<ResolutionScenario> =
        Gen.oneof
            [
                genTopLevelReferenceScenario |> Gen.map ResolutionScenario.TopLevelReference
                genNestedReferenceScenario |> Gen.map ResolutionScenario.NestedReference
                genForwardedTopLevelScenario |> Gen.map ResolutionScenario.ForwardedTopLevel
                genForwardedNestedScenario |> Gen.map ResolutionScenario.ForwardedNested
            ]

type AssemblyGraphArbitraries =
    static member TopLevelReferenceScenario () : Arbitrary<TopLevelReferenceScenario> =
        Arb.fromGen AssemblyGraphShape.genTopLevelReferenceScenario

    static member NestedReferenceScenario () : Arbitrary<NestedReferenceScenario> =
        Arb.fromGen AssemblyGraphShape.genNestedReferenceScenario

    static member ForwardedTopLevelScenario () : Arbitrary<ForwardedTopLevelScenario> =
        Arb.fromGen AssemblyGraphShape.genForwardedTopLevelScenario

    static member ForwardedNestedScenario () : Arbitrary<ForwardedNestedScenario> =
        Arb.fromGen AssemblyGraphShape.genForwardedNestedScenario

    static member ResolutionScenario () : Arbitrary<ResolutionScenario> =
        Arb.fromGen AssemblyGraphShape.genResolutionScenario
