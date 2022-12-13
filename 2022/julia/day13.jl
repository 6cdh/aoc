# val = int | list;
# int = ('0'..'9')+;
# list = "[", (val, ",")*, [val] "]";
function parse_int(line, i)
    if isdigit(line[i])
        val = 0
        while i <= length(line) && isdigit(line[i])
            val = val * 10 + parse(Int, line[i])
            i += 1
        end
        (val, i)
    else
        nothing
    end
end

function parse_list(line, i)
    if line[i] == '['
        i += 1
        list = []
        while true
            if line[i] in ", "
                i += 1
            elseif line[i] == ']'
                i += 1
                break
            else
                val, newi = parse_val(line, i)
                i = newi
                push!(list, val)
            end
        end
        (list, i)
    else
        nothing
    end
end

function parse_val(line, i)
    result = parse_int(line, i)
    if isnothing(result)
        parse_list(line, i)
    else
        result
    end
end

function parse_input()
    lines = readlines()

    pairs = []
    for line in lines
        if !isempty(line)
            pair, _ = parse_val(line, 1)
            push!(pairs, pair)
        end
    end
    pairs
end

function smaller_rec(p1, p2, i1, i2)
    if isa(p1, Int) && isa(p2, Int)
        cmp(p1, p2)
    elseif isa(p1, Int)
        smaller([p1], p2)
    elseif isa(p2, Int)
        smaller(p1, [p2])
    elseif i1 == length(p1) + 1 && i2 == length(p2) + 1
        0
    elseif i1 == length(p1) + 1
        -1
    elseif i2 == length(p2) + 1
        1
    else
        cmp1 = smaller(p1[i1], p2[i2])
        if cmp1 != 0
            cmp1
        else
            smaller_rec(p1, p2, i1 + 1, i2 + 1)
        end
    end
end

function smaller(p1, p2)
    smaller_rec(p1, p2, 1, 1)
end

function day13()
    packets = parse_input()

    # part 1
    cnt = 0
    for i in 1:2:length(packets)
        p1 = packets[i]
        p2 = packets[i+1]
        if smaller(p1, p2) == -1
            cnt += (i + 1) ÷ 2
        end
    end
    println(cnt)

    # part 2
    pkts = copy(packets)
    divider1 = [[2]]
    divider2 = [[6]]
    push!(pkts, divider1)
    push!(pkts, divider2)
    sorted = sort(pkts, lt=(a, b) -> smaller(a, b) == -1)
    idx1 = findfirst(x -> x == divider1, sorted)
    idx2 = findfirst(x -> x == divider2, sorted)

    println(idx1 * idx2)
end

@time day13()
