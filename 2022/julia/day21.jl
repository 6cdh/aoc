using Match

# x op y = result
function solve_relation(x, op, y, result)
    # map op to op1 that x = result op1 y
    op1 = Dict([
        (+, -),
        (-, +),
        (*, ÷),
        (÷, *),
        (==, (r, x) -> x)])
    # map op to op2 that y = result op2 x
    op2 = Dict([
        (+, -),
        (-, (r, x) -> x - r),
        (*, ÷),
        (÷, (r, x) -> x ÷ r),
        (==, (r, x) -> x)])

    if isnothing(result) && !isnothing(x) && !isnothing(y)
        op(x, y)
    elseif !isnothing(result) && isnothing(x) && !isnothing(y)
        op1[op](result, y)
    elseif !isnothing(result) && !isnothing(x) && isnothing(y)
        op2[op](result, x)
    else
        nothing
    end
end

function readinput()
    lines = readlines()

    monkeys = Dict()
    for line in lines
        words = split(line)
        if length(words) == 2
            monkeys[words[1][1:end-1]] = parse(Int, words[2])
        else
            fn =
                @match words[3] begin
                    "+" => +
                    "-" => -
                    "*" => *
                    "/" => ÷
                    op => error("unknown operator: ", op)
                end
            monkeys[words[1][1:end-1]] = (fn, words[2], words[4])
        end
    end
    monkeys
end

function get_answer(name, monkeys)
    action = get(monkeys, name, nothing)
    if isnothing(action)
        nothing
    elseif isa(action, Int)
        action
    else
        ans1 = get_answer(action[2], monkeys)
        ans2 = get_answer(action[3], monkeys)
        solve_relation(ans1, action[1], ans2, nothing)
    end
end

function solve_to_meet(name, monkeys, expect)
    action = get(monkeys, name, nothing)
    if isnothing(action)
        expect
    elseif isa(action, Int)
        action
    else
        ans1 = get_answer(action[2], monkeys)
        ans2 = get_answer(action[3], monkeys)
        if !isnothing(ans1) && !isnothing(ans2)
            solve_relation(ans1, action[1], ans2, nothing)
        elseif isnothing(ans1)
            ans1_expect = solve_relation(ans1, action[1], ans2, expect)
            solve_to_meet(action[2], monkeys, ans1_expect)
        elseif isnothing(ans2)
            ans2_expect = solve_relation(ans1, action[1], ans2, expect)
            solve_to_meet(action[3], monkeys, ans2_expect)
        else
            error("NO! both operands depend on `humn`")
        end
    end
end

function day21()
    monkeys = readinput()

    println(get_answer("root", monkeys))

    monkeys["root"] = (==, monkeys["root"][2], monkeys["root"][3])
    delete!(monkeys, "humn")

    println(solve_to_meet("root", monkeys, true))
end

@time day21()
