function readinput()
    parse.(Int, readlines())
end

function next_pos(pos, n, len)
    Δ = n % (len - 1)
    if 2 - pos <= Δ <= len - pos
        pos + Δ
    elseif Δ < 2 - pos
        pos + Δ + (len - 1)
    else
        pos + Δ - (len - 1)
    end
end

function mix(arr, k)
    len = length(arr)
    cur_index::Vector{Int} = [i for i in 1:len]
    # O (k * len^2)
    for _ in 1:k
        for i in 1:len
            pos = cur_index[i]
            val = arr[i]
            new_pos = next_pos(pos, val, len)

            if pos < new_pos
                for j in 1:len
                    if pos < cur_index[j] <= new_pos
                        cur_index[j] -= 1
                    end
                end
            elseif pos > new_pos
                for j in 1:len
                    if new_pos <= cur_index[j] < pos
                        cur_index[j] += 1
                    end
                end
            end
            cur_index[i] = new_pos
        end
    end
    mixed = fill(0, len)
    for i in 1:len
        mixed[cur_index[i]] = arr[i]
    end
    mixed
end

function next_nth_pos(pos, n, len)
    Δ = n % len
    (pos + Δ - 1) % len + 1
end

function build_answer(mixed_arr)
    n = length(mixed_arr)
    pos0 = findfirst(==(0), mixed_arr)
    pos1000 = next_nth_pos(pos0, 1000, n)
    pos2000 = next_nth_pos(pos0, 2000, n)
    pos3000 = next_nth_pos(pos0, 3000, n)
    sum([mixed_arr[pos1000], mixed_arr[pos2000], mixed_arr[pos3000]])
end

function day20()
    arr = readinput()
    println(build_answer(mix(arr, 1)))

    decrypted = map(x -> x * 811589153, arr)
    println(build_answer(mix(decrypted, 10)))
end

@time day20()
