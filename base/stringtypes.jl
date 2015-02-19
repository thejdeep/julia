## basic string types needed to bootstrap other files ##

immutable StringIndex
    i::Int
end

# Commented out because it makes the build fail with an error about < below
#function StringIndex(i::Int)
#    i > 0 || error("index must be > 0")
#    StringIndex(i)
#end

# Ints implements those, the code relies on it in some places (e.g. split, see FIXME)
first(i::StringIndex) = i
last(i::StringIndex) = i


#<(i::StringIndex, j::StringIndex) = error("cannot compare two StringIndex objects of different types")
<(i::StringIndex, j::StringIndex) = i.i < j.i

+(i::StringIndex, j::Integer) = error("+(::StringIndex, ::Integer) not implemented: use nextind or +(::StringIndex, ::CodeUnit) instead")
-(i::StringIndex, j::Integer) = error("-(::StringIndex, ::Integer) not implemented: use prevind or -(::StringIndex, ::CodeUnit) instead")
#+(i::StringIndex, j::Integer) = error("index arithmetic is only supported for DirectIndexString")
#-(i::StringIndex, j::Integer) = error("index arithmetic is only supported for DirectIndexString")
#+{S<:DirectIndexString}(i::StringIndex{S}, j::Integer) = error("not implemented")
#+{S<:DirectIndexString}(i::StringIndex{S}, j::Integer) = error("not implemented")


immutable CodeUnit
    i::Int
end

codeunit = CodeUnit(1)
+(i::StringIndex, j::CodeUnit) = StringIndex(i.i+j.i)
+(i::CodeUnit, j::StringIndex) = StringIndex(i.i+j.i)
-(i::StringIndex, j::CodeUnit) = StringIndex(i.i-j.i)
*(i::CodeUnit, j::Integer) = CodeUnit(i.i*j)
*(i::Integer, j::CodeUnit) = CodeUnit(j.i*i)

# Inheriting from Range triggers a crash with length()
immutable StringRange# <: Range{StringIndex}
    start::StringIndex
    stop::StringIndex
end

#colon{S<:AbstractString}(start::StringIndex{S}, stop::StringIndex{S}) = StringRange(start, stop)
colon(start::StringIndex, stop::StringIndex) = StringRange(start, stop)
function colon(start::Integer, stop::StringIndex)
    start == 1 || error("StringRange can only be constructed from StringIndex position or 1")
    StringRange(StringIndex(1), stop)
end

colon(start::CodeUnit, stop::StringIndex) = StringRange(StringIndex(start.i), stop)
colon(start::StringIndex, stop::CodeUnit) = StringRange(start, StringIndex(stop.i))

# Does not make sense without a reference to the underlying string
# FIXME: triggers ambiguity warnings
length(r::StringRange) = error("cannot compute length of StringRange")
start(r::StringRange) = error("cannot iterate over StringRange")
#getindex(r::StringRange, ::Real) = error("not implemented")
#getindex(r::StringRange, ::AbstractVector{Bool}) = error("not implemented")
#getindex{T<:Real}(r::StringRange, ::AbstractVector{T}) = error("not implemented")
#getindex(r::StringRange, ::AbstractArray) = error("not implemented")
#getindex(r::StringRange, ::Any) = error("not implemented")

first(r::StringRange) = r.start
last(r::StringRange) = r.stop
isempty(r::StringRange) = r.start > r.stop
