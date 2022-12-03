app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task]
    provides [main] to pf

Bag : { first: Set U32, second: Set U32 }

score : U32 -> U32
score = \scalar ->
    if scalar < 97 then
        scalar - 65 + 27
    else
        scalar - 97 + 1
expect score 97 == 1
expect score 65 == 27

accumulateRow : Str -> Bag
accumulateRow = \row ->
    scalars = row |> Str.toScalars
    { before, others } = List.split scalars ((List.len scalars |> Num.toFrac) / 2 |> Num.floor)
    {
        first: before |> List.map score |> Set.fromList,
        second: others |> List.map score |> Set.fromList,
    }

expect accumulateRow "abcd" == { first: Set.fromList [1, 2], second: Set.fromList [3, 4] }
expect accumulateRow "pLPv" == { first: Set.fromList [16, 38], second: Set.fromList [42, 22] }

scoreRow : Bag -> U32
scoreRow = \{ first, second } ->
    Set.intersection first second |> Set.toList |> List.sum

processRow : Str -> U32
processRow = \row ->
    row |> accumulateRow |> scoreRow

expect processRow "vJrwpWtwJgWrhcsFMMfFFhFp" == 16

part1 : Str -> U32
part1 = \file -> 
    file
    |> Str.split "\n"
    |> List.map processRow
    |> List.sum

exampleInput =
"""
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

expect part1 exampleInput == 157

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day2.txt"))
    count = Result.map result \file -> { first: part1 file }
    when count is
        Ok { first } -> 
            first |> Num.toStr |> Stdout.line
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line
