app "day5"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task]
    provides [main] to pf

parseStackRow : Str -> List (Result U8 [None])
parseStackRow = \row ->
    bytes = row |> Str.toUtf8
    List.range { start: At 0, end: Before (((bytes |> List.len) + 1) |> Num.divTrunc 4) }
    |> List.map \index ->
        List.get bytes (index * 4 + 1)
        |> Result.mapErr \_ -> None
        |> Result.try \val ->
            if val == ' ' then
                Err None
            else
                Ok val

expect parseStackRow "[Z] [M] [P]" == [Ok 'Z', Ok 'M', Ok 'P']
expect parseStackRow "[Z] [M]" == [Ok 'Z', Ok 'M']
expect parseStackRow "    [D]    " == [Err None, Ok 'D', Err None]
expect parseStackRow "[W]     [J] [L]" == [Ok 'W', Err None, Ok 'J', Ok 'L']

transpose : List (List t) -> List (List t)
transpose = \matrix ->
    height = List.len matrix
    width = matrix |> List.get 0 |> Result.map List.len |> Result.withDefault 0 # Assumes proper rectangular matrix
    List.range { start: At 0, end: Before width }
    |> List.map \col ->
        List.range { start: At 0, end: Before height }
        |> List.keepOks \row ->
            matrix
            |> List.get row
            |> Result.withDefault []
            |> List.get col

expect transpose [[1, 2, 3], [4, 5, 6]] == [[1, 4], [2, 5], [3, 6]]
expect transpose [[1, 2, 3, 7], [4, 5, 6, 8]] == [[1, 4], [2, 5], [3, 6], [7, 8]]

parseStacks : Str -> List (List U8)
parseStacks = \input ->
    input
    |> Str.split "\n"
    |> List.map parseStackRow
    |> transpose
    |> List.map List.reverse
    |> List.map \col -> List.keepOks col \a -> a

exampleStacks =
"""
    [D]    
[N] [C]    
[Z] [M] [P]
"""

expect parseStacks exampleStacks == [['Z', 'N'], ['M', 'C', 'D'], ['P']]

exampleInput =
"""
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

splitInput : Str -> { stacks: Str, commands: Str }
splitInput = \input ->
    when Str.split input "\n\n" is
        [stacks, commands] ->
            {
                stacks: Str.splitLast stacks "\n" |> Result.map (\r -> r.before) |> Result.withDefault "",
                commands,
            }
        _ -> crash "Bad input"

expect splitInput exampleInput == {
    stacks:
    """
        [D]    
    [N] [C]    
    [Z] [M] [P]
    """,
    commands:
    """
    move 1 from 2 to 1
    move 3 from 1 to 3
    move 2 from 2 to 1
    move 1 from 1 to 2
    """
}

Command : {
    source: Nat,
    destination: Nat,
    amount: Nat,
}

parseCommand : Str -> Command
parseCommand = \raw ->
    when Str.split raw " " is
        [_, amount, _, source, _, destination] -> {
                source: source |> Str.toNat |> Result.withDefault 0,
                amount: amount |> Str.toNat |> Result.withDefault 0,
                destination: destination |> Str.toNat |> Result.withDefault 0,
            }
        _ -> crash "bad row"

expect parseCommand "move 1 from 2 to 1" == { source: 2, destination: 1, amount: 1}

Mode : [OneAtATime, AllTogether]

runCommand : List (List U8), Command, Mode -> List (List U8)
runCommand = \stacks, command, mode ->
    stacks |> List.mapWithIndex \col, index ->
        if index == (command.source - 1) then
            col |> List.takeFirst ((col |> List.len) - command.amount)
        else if index == (command.destination - 1) then
            incomingCrates = (stacks |> List.get (command.source - 1) |> Result.withDefault [] |> List.takeLast command.amount)
            col |> List.concat (
                when mode is
                    OneAtATime -> incomingCrates |> List.reverse
                    AllTogether -> incomingCrates
            ) 
        else
            col

expect runCommand [[1, 2, 3], [4, 5, 6], [7]] { source: 1, destination: 3, amount: 2} OneAtATime  == [[1], [4, 5, 6], [7, 3, 2]]
expect runCommand [[1, 2, 3], [4, 5, 6], [7]] { source: 2, destination: 3, amount: 3}  OneAtATime == [[1, 2, 3], [], [7, 6, 5, 4]]
expect runCommand [['Z', 'N', 'D'], ['M', 'C'], ['P']] { source: 1, destination: 3, amount: 3 } OneAtATime == [[], ['M', 'C'], ['P', 'D', 'N', 'Z']]
expect runCommand [[], ['M', 'C'], ['P', 'D', 'N', 'Z']] { source: 2, destination: 1, amount: 2 } OneAtATime == [['C', 'M'], [], ['P', 'D', 'N', 'Z']]

processFile : Str, Mode -> Str
processFile = \input, mode ->
    { stacks, commands } = splitInput input
    commands
    |> Str.split "\n"
    |> List.map parseCommand
    |> List.walk (parseStacks stacks) \acc, cmd -> runCommand acc cmd mode
    |> List.keepOks List.last
    |> List.keepOks \val -> Str.fromUtf8 [val]
    |> List.walk "" Str.concat

expect processFile exampleInput OneAtATime == "CMZ"
expect processFile exampleInput AllTogether == "MCD"

part1 = \input -> processFile input OneAtATime
part2 = \input -> processFile input AllTogether

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day5.txt"))
    count = Result.map result \file -> { first: part1 file, second: part2 file }
    when count is
        Ok { first, second } -> 
            _ <- first |> Stdout.line |> Task.await
            second |> Stdout.line
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line


