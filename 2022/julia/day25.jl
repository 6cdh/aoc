function snafu2int(snafu)
    ans = 0
    n = length(snafu)
    sym2int = Dict(['=' => -2, '-' => -1, '0' => 0, '1' => 1, '2' => 2])
    for i in n:-1:1
        ans += sym2int[snafu[i]] * 5^(n - i)
    end
    ans
end

function int2snafu(v)
    base5 = string(v, base=5) |> collect |> reverse
    carry = 0
    for i in 1:length(base5)
        cur = base5[i] + carry
        base5[i] =
            if cur < '3'
                carry = 0
                cur
            elseif cur == '3'
                carry = 1
                '='
            elseif cur == '4'
                carry = 1
                '-'
            elseif cur == '5'
                carry = 1
                '0'
            end
    end
    if carry == 1
        push!(base5, '1')
    end
    join(base5) |> reverse
end

function day25()
    lines = readlines()

    println(sum(map(snafu2int, lines)) |> int2snafu)
end

@time day25()
