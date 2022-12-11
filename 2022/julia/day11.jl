using Match

mutable struct Monkey
    items::Vector{Int}
    operation::Function
    test::Int
    iftrue::Int
    iffalse::Int
    inspection::Int
end

function parse_op(ops)
    @match ops begin
        ["new", "=", "old", "+", "old"] => x -> x + x
        ["new", "=", "old", "*", "old"] => x -> x * x
        ["new", "=", "old", "+", key] => x -> x + parse(Int, key)
        ["new", "=", "old", "*", key] => x -> x * parse(Int, key)
        _ => error("unexpected operation")
    end
end

function read_monkeys()
    lines = readlines()
    monkeys::Vector{Monkey} = []
    for line in lines
        @match split(line) begin
            ["Monkey", _] => begin
                push!(monkeys, Monkey([], identity, 1, 0, 0, 0))
            end
            ["Starting", "items:", its...] => begin
                last(monkeys).items = map(s -> parse(Int, strip(s, ',')), its)
            end
            ["Operation:", ops...] => begin
                last(monkeys).operation = parse_op(ops)
            end
            ["Test:", _..., test] => begin
                last(monkeys).test = parse(Int, test)
            end
            ["If", "true:", _..., iftrue] => begin
                last(monkeys).iftrue = parse(Int, iftrue) + 1
            end
            ["If", "false:", _..., iffalse] => begin
                last(monkeys).iffalse = parse(Int, iffalse) + 1
            end
        end
    end
    monkeys
end

function round1!(monkeys, manager)
    for m in monkeys
        for it in m.items
            new = manager(m.operation(it))
            m.inspection += 1
            if new % m.test == 0
                push!(monkeys[m.iftrue].items, new)
            else
                push!(monkeys[m.iffalse].items, new)
            end
        end
        m.items = []
    end
end

function rounds!(monkeys, k, manager)
    for _ in 1:k
        round1!(monkeys, manager)
    end
    sorted = sort(map(m -> m.inspection, monkeys), rev=true)
    sorted[1] * sorted[2]
end

function day11()
    monkeys = read_monkeys()
    println(rounds!(deepcopy(monkeys), 20, x -> floor(Int, x / 3)))
    lcmv = lcm(map(m -> m.test, monkeys))
    println(rounds!(deepcopy(monkeys), 10000, x -> x % lcmv))
end

@time day11()
