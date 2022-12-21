using Match

# try to solve the unknown variable given 2 of 3 variables
# or return nothing if it can't
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

function solve_equations(name, monkeys, expect)
    action = get(monkeys, name, nothing)
    if isnothing(action)
        expect
    elseif isa(action, Int)
        action
    elseif isnothing(expect)
        solve_relation(
            solve_equations(action[2], monkeys, nothing),
            action[1],
            solve_equations(action[3], monkeys, nothing),
            nothing)
    else
        ans1 = solve_equations(action[2], monkeys, nothing)
        ans2 = solve_equations(action[3], monkeys, nothing)
        @assert(!(isnothing(ans1) && isnothing(ans2)))
        if isnothing(ans1)
            ans1_expect = solve_relation(ans1, action[1], ans2, expect)
            solve_equations(action[2], monkeys, ans1_expect)
        elseif isnothing(ans2)
            ans2_expect = solve_relation(ans1, action[1], ans2, expect)
            solve_equations(action[3], monkeys, ans2_expect)
        else
            error("NO!!!! both operands depend on `humn`")
        end
    end
end

function day21()
    monkeys = readinput()

    println(solve_equations("root", monkeys, nothing))

    root_action = monkeys["root"]
    monkeys["root"] = (==, root_action[2], root_action[3])
    delete!(monkeys, "humn")

    println(solve_equations("root", monkeys, true))
end

@time day21()
