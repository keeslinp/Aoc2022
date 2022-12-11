app "day4"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task]
    provides [main] to pf

Range : { lower: U32, upper: U32 }

parseRange : Str -> Result Range [InvalidNumStr, BadFormat]
parseRange = \range ->
    vals <- range |> Str.split "-" |> List.mapTry Str.toU32 |> Result.try
    when vals is
        [lower, upper] -> Ok { lower, upper }
        _ -> Err BadFormat

expect parseRange "2-4" == Ok { lower: 2,  upper: 4 }
expect parseRange "2-a" == Err InvalidNumStr
expect parseRange "2-3-4" == Err BadFormat

parseLine : Str -> Result (List Range) [InvalidNumStr, BadFormat]
parseLine = \line ->
    line
    |> Str.split "," 
    |> List.mapTry parseRange

expect parseLine "2-4,6-8" == Ok [{ lower: 2, upper: 4 }, { lower: 6, upper: 8 }]
expect parseLine "2-4,a-8" == Err InvalidNumStr

isFullyContained : Range, Range -> Bool
isFullyContained = \first, second ->
    first.lower >= second.lower && first.upper <= second.upper

expect isFullyContained { lower: 3, upper: 5} {lower: 2, upper: 7} == Bool.true
expect isFullyContained { lower: 1, upper: 5} {lower: 2, upper: 7} == Bool.false

isOverlapped : Range, Range -> Bool
isOverlapped = \first, second ->
    (first.lower >= second.lower && first.lower <= second.upper)
    || (first.upper >= second.lower && first.upper <= second.upper)
    || (second.lower >= first.lower && second.lower <= first.upper)
    || (second.upper >= first.lower && second.upper <= first.upper)
expect isOverlapped { lower: 1, upper: 3} {lower: 2, upper: 7} == Bool.true
expect isOverlapped { lower: 1, upper: 5} {lower: 6, upper: 7} == Bool.false
expect isOverlapped { lower: 2, upper: 8} {lower: 3, upper: 7} == Bool.true
expect isOverlapped { lower: 2, upper: 6} {lower: 4, upper: 8} == Bool.true
expect isOverlapped { lower: 6, upper: 6} {lower: 4, upper: 6} == Bool.true
expect isOverlapped { lower: 5, upper: 7} {lower: 7, upper: 9} == Bool.true


countCompletOverlaps : List (List Range) -> Nat
countCompletOverlaps = \lines ->
    lines |> List.countIf \ranges ->
        when ranges is
            [first, second] -> isFullyContained first second || isFullyContained second first
            _ -> Bool.false

expect countCompletOverlaps [[{ lower: 3, upper: 5}, { lower: 3, upper: 7}], [{ lower: 2, upper: 5}, { lower: 1, upper: 3}]] == 1

countOverlaps : List (List Range) -> Nat
countOverlaps = \lines ->
    lines |> List.countIf \ranges ->
        when ranges is
            [first, second] -> isOverlapped first second
            _ -> Bool.false

expect countOverlaps [[{ lower: 1, upper: 5}, { lower: 3, upper: 7}], [{ lower: 2, upper: 5}, { lower: 6, upper: 8}]] == 1


part1 : Str -> Result Nat [BadFormat, InvalidNumStr]
part1 = \file -> 
    file
        |> Str.split "\n"
        |> List.mapTry parseLine
        |> Result.map countCompletOverlaps

part2 : Str -> Result Nat [BadFormat, InvalidNumStr]
part2 = \file -> 
    file
        |> Str.split "\n"
        |> List.mapTry parseLine
        |> Result.map countOverlaps

exampleInput =
"""
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

expect part1 exampleInput == Ok 2
expect part2 exampleInput == Ok 4

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day4.txt"))
    count = Result.map result \file -> { first: part1 file, second: part2 file }
    when count is
        Ok { first, second } -> 
            _ <- first |> Result.withDefault 0 |> Num.toStr |> Stdout.line |> Task.await
            second |> Result.withDefault 0 |> Num.toStr |> Stdout.line
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line

