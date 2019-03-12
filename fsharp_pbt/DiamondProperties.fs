module DiamondProperties

open System
open FsCheck
open FsCheck.Xunit

let mutable count: int = 0

type Letters = 
    static member Char = 
        Arb.Default.Char()
        |> Arb.filter(fun c -> 'A' <= c && c <= 'Z')
    
type DiamondPropertyAttribute () = 
    inherit PropertyAttribute(Arbitrary = [|typeof<Letters>|])

[<Property>]
let ``Diamond is non-empty`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

[<Property(Arbitrary = [|typeof<Letters>|], MaxTest = 200)>]
let ``Diamond is non-empty - Version 3`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual) 
    |> Prop.collect(String.Format("value {0:000}: {1}", count, letter))

[<DiamondPropertyAttribute>]
let ``Diamond is non-empty - Version 2`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

let split (x: string) = 
    x.Split([|Environment.NewLine|], StringSplitOptions.None)

[<DiamondPropertyAttribute>]
let ``First row contains 'A'`` (letter: char) =
    let actual = Diamond.make letter
    
    let rows = split actual
    rows |> Seq.head |> Seq.exists ((=) 'A')

let leadingSpaces (x: string) =
    let indexOfNonSpace = x.IndexOfAny[|'A' .. 'Z'|]
    x.Substring(0, indexOfNonSpace)

let trailingSpaces (x: string) =
    let lastIndexOfNonSpace = x.LastIndexOfAny[|'A' .. 'Z'|]
    x.Substring(lastIndexOfNonSpace + 1)

[<DiamondPropertyAttribute>]
let ``All rows must have a symmetric contour`` (letter: char) =
    let actual = Diamond.make letter
    
    let rows = split actual
    rows |> Array.forall(fun r -> (leadingSpaces r) = (trailingSpaces r))