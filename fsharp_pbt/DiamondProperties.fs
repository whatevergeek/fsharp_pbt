module DiamondProperties

open System
open FsCheck
open FsCheck.Xunit
open System

let mutable count: int = 0

type Letters = 
    static member Char = 
        Arb.Default.Char()
        |> Arb.filter(fun c -> 'A' <= c && c <= 'Z')
    
type DiamondProperty () = 
    inherit PropertyAttribute(Arbitrary = [|typeof<Letters>|])

[<Property(Arbitrary = [|typeof<Letters>|], MaxTest = 200)>]
let ``Diamond is non-empty - Version 3`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual) 
    |> Prop.collect(String.Format("value {0:000}: {1}", count, letter))

[<DiamondProperty>]
let ``Diamond is non-empty - Version 2`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

let split (x: string) = 
    x.Split([|Environment.NewLine|], StringSplitOptions.None)

[<DiamondProperty>]
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

[<DiamondProperty>]
let ``All rows must have a symmetric contour`` (letter: char) =
    let actual = Diamond.make letter
    
    let rows = split actual
    rows |> Array.forall(fun r -> (leadingSpaces r) = (trailingSpaces r))

let trim (x: string) = x.Trim()

[<DiamondProperty>]
let ``Top of figure has correct letters in correct order`` (letter: char) =
    let actual = Diamond.make letter
    
    let expected = ['A' .. letter]
    let rows = split actual
    let firstNonWhiteSpaceLetters = 
        rows
        |> Seq.take expected.Length
        |> Seq.map trim
        |> Seq.map Seq.head
        |> Seq.toList
    expected = firstNonWhiteSpaceLetters

[<DiamondProperty>]
let ``Figure is symmetric around the horizontal axis`` (letter: char) =
    let actual = Diamond.make letter
    
    let rows = split actual
    let topRows = 
        rows
        |> Seq.takeWhile(fun x -> not (x.Contains(string letter)))
        |> Seq.toList
    let bottomRows = 
        rows
        |> Seq.skipWhile(fun x -> not (x.Contains(string letter)))
        |> Seq.skip 1
        |> Seq.toList
        |> List.rev
    topRows = bottomRows


[<DiamondProperty>]
let ``Diamond is as wide as its height`` (letter: char) =
    let actual = Diamond.make letter
    
    let rows = split actual
    let expected = rows.Length
    rows |> Array.forall(fun x -> x.Length = expected)