using Match

rock::Int = 0
paper::Int = 1
scissor::Int = 2

which_win(shape) = (shape + 1) % 3
which_lose(shape) = (shape + 3 - 1) % 3
score_of(shape) = shape + 1

function outcome(opp, my)
    if opp == my
        3
    elseif which_lose(my) == opp
        6
    else
        0
    end
end

function which_shape(opp, result)
    @match result begin
        'X' => which_lose(opp)
        'Y' => opp
        'Z' => which_win(opp)
    end
end

function day02()
    lines = readlines()

    score = 0
    score2 = 0

    for line in lines
        opp = Dict('A' => rock, 'B' => paper, 'C' => scissor)[line[1]]
        me = Dict('X' => rock, 'Y' => paper, 'Z' => scissor)[line[3]]

        me2 = which_shape(opp, line[3])

        score += score_of(me) + outcome(opp, me)
        score2 += score_of(me2) + outcome(opp, me2)
    end

    println(score)
    println(score2)
end

@time day02()
