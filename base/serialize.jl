## serializing values ##

# type Serializer  # defined in dict.jl

# dummy types to tell number of bytes used to store length (4 or 1)
abstract LongSymbol
abstract LongTuple
abstract LongExpr
abstract UndefRefTag
abstract BackrefTag

const ser_version = 2 # do not make changes without bumping the version #!
const ser_tag = ObjectIdDict()
const deser_tag = ObjectIdDict()
let i = 2
    global ser_tag, deser_tag
    for t = Any[
             Symbol, Int8, UInt8, Int16, UInt16, Int32, UInt32,
             Int64, UInt64, Int128, UInt128, Float32, Float64, Char, Ptr,
             DataType, UnionType, Function,
             Tuple, Array, Expr, LongSymbol, LongTuple, LongExpr,
             LineNumberNode, SymbolNode, LabelNode, GotoNode,
             QuoteNode, TopNode, TypeVar, Box, LambdaStaticData,
             Module, UndefRefTag, Task, ASCIIString, UTF8String,
             UTF16String, UTF32String, Float16,
             BackrefTag, :reserved10, :reserved11, :reserved12,

             (), Bool, Any, :Any, Bottom, :reserved21, :reserved22, Type,
             :Array, :TypeVar, :Box,
             :lambda, :body, :return, :call, symbol("::"),
             :(=), :null, :gotoifnot, :A, :B, :C, :M, :N, :T, :S, :X, :Y,
             :a, :b, :c, :d, :e, :f, :g, :h, :i, :j, :k, :l, :m, :n, :o,
             :p, :q, :r, :s, :t, :u, :v, :w, :x, :y, :z,
             :add_int, :sub_int, :mul_int, :add_float, :sub_float,
             :mul_float, :unbox, :box,
             :eq_int, :slt_int, :sle_int, :ne_int,
             :arrayset, :arrayref,
             :Core, :Base, :reserved15, :reserved16,
             :reserved17, :reserved18, :reserved19, :reserved20,
             false, true, nothing, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
             12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
             28, 29, 30, 31, 32]
        ser_tag[t] = int32(i)
        deser_tag[int32(i)] = t
        i += 1
    end
end

# tags >= this just represent themselves, their whole representation is 1 byte
const VALUE_TAGS = ser_tag[()]

const EMPTY_TUPLE_TAG = ser_tag[()]
const ZERO_TAG = ser_tag[0]
const INT_TAG = ser_tag[Int]

writetag(s::IO, x) = write(s, uint8(ser_tag[x]))

function write_as_tag(s::IO, x)
    t = ser_tag[x]
    if t < VALUE_TAGS
        write(s, uint8(0))
    end
    write(s, uint8(t))
end

# cycle handling
function serialize_cycle(s::Serializer, x)
    if !isdefined(s,:table)
        s.table = ObjectIdDict()
    end
    offs = get(s.table, x, -1)
    if offs != -1
        writetag(s.io, BackrefTag)
        write(s.io, int(offs))
        return true
    end
    s.table[x] = position(s.io)
    return false
end

serialize(s::Serializer, x::Bool) = write_as_tag(s.io, x)

serialize(s::Serializer, ::Ptr) = error("cannot serialize a pointer")

serialize(s::Serializer, ::()) = write(s.io, uint8(EMPTY_TUPLE_TAG)) # write_as_tag(s, ())

function serialize(s::Serializer, t::Tuple)
    l = length(t)
    if l <= 255
        writetag(s.io, Tuple)
        write(s.io, uint8(l))
    else
        writetag(s.io, LongTuple)
        write(s.io, int32(l))
    end
    for i = 1:l
        serialize(s, t[i])
    end
end

function serialize(s::Serializer, x::Symbol)
    if haskey(ser_tag, x)
        return write_as_tag(s.io, x)
    end
    pname = convert(Ptr{UInt8}, x)
    ln = int(ccall(:strlen, Csize_t, (Ptr{UInt8},), pname))
    if ln <= 255
        writetag(s.io, Symbol)
        write(s.io, uint8(ln))
    else
        writetag(s.io, LongSymbol)
        write(s.io, int32(ln))
    end
    write(s.io, pname, ln)
end

function serialize_array_data(s::IO, a)
    elty = eltype(a)
    if elty === Bool && length(a)>0
        last = a[1]
        count = 1
        for i = 2:length(a)
            if a[i] != last || count == 127
                write(s, uint8((uint8(last)<<7) | count))
                last = a[i]
                count = 1
            else
                count += 1
            end
        end
        write(s, uint8((uint8(last)<<7) | count))
    else
        write(s, a)
    end
end

function serialize(s::Serializer, a::Array)
    serialize_cycle(s, a) && return
    elty = eltype(a)
    writetag(s.io, Array)
    if elty !== UInt8
        serialize(s, elty)
    end
    if ndims(a) != 1
        serialize(s, size(a))
    else
        serialize(s, length(a))
    end
    if isbits(elty)
        serialize_array_data(s.io, a)
    else
        for i = 1:length(a)
            if isdefined(a, i)
                serialize(s, a[i])
            else
                writetag(s.io, UndefRefTag)
            end
        end
    end
end

function serialize{T,N,A<:Array}(s::Serializer, a::SubArray{T,N,A})
    serialize_cycle(s, a) && return
    if !isbits(T) || stride(a,1)!=1
        return serialize(s, copy(a))
    end
    writetag(s.io, Array)
    serialize(s, T)
    serialize(s, size(a))
    serialize_array_data(s.io, a)
end

function serialize(s::Serializer, e::Expr)
    serialize_cycle(s, e) && return
    l = length(e.args)
    if l <= 255
        writetag(s.io, Expr)
        write(s.io, uint8(l))
    else
        writetag(s.io, LongExpr)
        write(s.io, int32(l))
    end
    serialize(s, e.head)
    serialize(s, e.typ)
    for a = e.args
        serialize(s, a)
    end
end

function serialize_mod_names(s, m::Module)
    if m !== Main
        serialize_mod_names(s, module_parent(m))
        serialize(s, module_name(m))
    end
end

function serialize(s::Serializer, m::Module)
    writetag(s.io, Module)
    serialize_mod_names(s, m)
    serialize(s, ())
    nothing
end

function serialize(s::Serializer, f::Function)
    serialize_cycle(s, f) && return
    writetag(s.io, Function)
    name = false
    if isgeneric(f)
        name = f.env.name
    elseif isa(f.env,Symbol)
        name = f.env
    end
    if isa(name,Symbol)
        if isdefined(Base,name) && is(f,eval(Base,name))
            write(s.io, uint8(0))
            serialize(s, name)
            return
        end
        mod = ()
        if isa(f.env,Symbol)
            mod = Core
        elseif !is(f.env.defs, ())
            mod = f.env.defs.func.code.module
        end
        if mod !== ()
            if isdefined(mod,name) && is(f,eval(mod,name))
                # toplevel named func
                write(s.io, uint8(2))
                serialize(s, mod)
                serialize(s, name)
                return
            end
        end
        write(s.io, uint8(3))
        serialize(s, f.env)
    else
        linfo = f.code
        @assert isa(linfo,LambdaStaticData)
        write(s.io, uint8(1))
        serialize(s, linfo)
        serialize(s, f.env)
    end
end

const lambda_numbers = WeakKeyDict()
lnumber_salt = 0
function lambda_number(l::LambdaStaticData)
    global lnumber_salt, lambda_numbers
    if haskey(lambda_numbers, l)
        return lambda_numbers[l]
    end
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    ln = lnumber_salt+(uint64(myid())<<44)
    lnumber_salt += 1
    lambda_numbers[l] = ln
    return ln
end

function serialize(s::Serializer, linfo::LambdaStaticData)
    serialize_cycle(s, linfo) && return
    writetag(s.io, LambdaStaticData)
    serialize(s, lambda_number(linfo))
    serialize(s, uncompressed_ast(linfo))
    if isdefined(linfo.def, :roots)
        serialize(s, linfo.def.roots)
    else
        serialize(s, [])
    end
    serialize(s, linfo.sparams)
    serialize(s, linfo.inferred)
    serialize(s, linfo.module)
    if isdefined(linfo, :capt)
        serialize(s, linfo.capt)
    else
        serialize(s, nothing)
    end
end

function serialize(s::Serializer, t::Task)
    serialize_cycle(s, t) && return
    if istaskstarted(t) && !istaskdone(t)
        error("cannot serialize a running Task")
    end
    writetag(s.io, Task)
    serialize(s, t.code)
    serialize(s, t.storage)
    serialize(s, t.state == :queued || t.state == :waiting ? (:runnable) : t.state)
    serialize(s, t.result)
    serialize(s, t.exception)
end

function serialize_type_data(s, t)
    tname = t.name.name
    serialize(s, tname)
    mod = t.name.module
    serialize(s, mod)
    if t.parameters !== ()
        if isdefined(mod,tname) && is(t,eval(mod,tname))
            serialize(s, ())
        else
            serialize(s, t.parameters)
        end
    end
end

function serialize(s::Serializer, t::DataType)
    if haskey(ser_tag,t)
        write_as_tag(s.io, t)
    else
        writetag(s.io, DataType)
        write(s.io, uint8(0))
        serialize_type_data(s, t)
    end
end

function serialize_type(s, t::DataType)
    if haskey(ser_tag,t)
        writetag(s.io, t)
    else
        writetag(s.io, DataType)
        write(s.io, uint8(1))
        serialize_type_data(s, t)
    end
end

function serialize(s::Serializer, n::Int)
    if 0 <= n <= 32
        write(s.io, uint8(ZERO_TAG+n))
        return
    end
    write(s.io, uint8(INT_TAG))
    write(s.io, n)
    nothing
end

serialize(s::IO, x) = serialize(Serializer(s), x)

function serialize(s::Serializer, x)
    if haskey(ser_tag,x)
        return write_as_tag(s.io, x)
    end
    t = typeof(x)
    if length(t.names)==0 && t.size>0
        serialize_type(s, t)
        write(s.io, x)
    else
        if !isimmutable(x)
            serialize_cycle(s, x) && return
        end
        serialize_type(s, t)
        for i in 1:length(t.names)
            if isdefined(x, i)
                serialize(s, getfield(x, i))
            else
                writetag(s.io, UndefRefTag)
            end
        end
    end
end

## deserializing values ##

deserialize(s::IO) = deserialize(Serializer(s))

function deserialize(s::Serializer)
    handle_deserialize(s, int32(read(s.io, UInt8)))
end

function deserialize_cycle(s::Serializer, x, pos)
    pos < 0 && return
    if !isdefined(s, :table)
        s.table = ObjectIdDict()
    end
    s.table[pos] = x
end

# deserialize_ is an internal function to dispatch on the tag
# describing the serialized representation. the number of
# representations is fixed, so deserialize_ does not get extended.
function handle_deserialize(s, b)
    last_pos = position(s.io)-1
    if b == 0
        return deser_tag[int32(read(s.io, UInt8))]
    end
    tag = deser_tag[b]
    if b >= VALUE_TAGS
        return tag
    elseif tag === Tuple
        len = int32(read(s.io, UInt8))
        return deserialize_tuple(s, len)
    elseif tag === LongTuple
        len = read(s.io, Int32)
        return deserialize_tuple(s, len)
    elseif tag === BackrefTag
        pos = read(s.io, Int)
        return s.table[pos]
    end
    return deserialize_(s, tag, last_pos)
end

deserialize_tuple(s, len) = ntuple(len, i->deserialize(s))

deserialize_(s, ::Type{Symbol}, p) = symbol(read(s.io, UInt8, int32(read(s.io, UInt8))))
deserialize_(s, ::Type{LongSymbol}, p) = symbol(read(s.io, UInt8, read(s.io, Int32)))

function deserialize_(s, ::Type{Module}, p)
    path = deserialize(s)
    m = Main
    if isa(path,Tuple) && path !== ()
        # old version
        for mname in path
            if !isdefined(m,mname)
                warn("Module $mname not defined on process $(myid())")  # an error seemingly fails
            end
            m = eval(m,mname)::Module
        end
    else
        mname = path
        while mname !== ()
            if !isdefined(m,mname)
                warn("Module $mname not defined on process $(myid())")  # an error seemingly fails
            end
            m = eval(m,mname)::Module
            mname = deserialize(s)
        end
    end
    m
end

const known_lambda_data = Dict()

function deserialize_(s, ::Type{Function}, pos)
    b = read(s.io, UInt8)
    if b==0
        name = deserialize(s)::Symbol
        if !isdefined(Base,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return eval(Base,name)::Function
    elseif b==2
        mod = deserialize(s)::Module
        name = deserialize(s)::Symbol
        if !isdefined(mod,name)
            return (args...)->error("function $name not defined on process $(myid())")
        end
        return eval(mod,name)::Function
    elseif b==3
        env = deserialize(s)
        return ccall(:jl_new_gf_internal, Any, (Any,), env)::Function
    end
    linfo = deserialize(s)
    f = ccall(:jl_new_closure, Any, (Ptr{Void}, Ptr{Void}, Any), C_NULL, C_NULL, linfo)::Function
    deserialize_cycle(s, f, pos)
    f.env = deserialize(s)
    return f
end

function deserialize_(s, ::Type{LambdaStaticData}, pos)
    lnumber = deserialize(s)
    if haskey(known_lambda_data, lnumber)
        linfo = known_lambda_data[lnumber]::LambdaStaticData
        makenew = false
    else
        linfo = ccall(:jl_new_lambda_info, Any, (Ptr{Void}, Ptr{Void}), C_NULL, C_NULL)::LambdaStaticData
        makenew = true
    end
    deserialize_cycle(s, linfo, pos)
    if makenew
        ast = deserialize(s)
        roots = deserialize(s)
        sparams = deserialize(s)
        infr = deserialize(s)
        mod = deserialize(s)
        capt = deserialize(s)
        linfo.ast = ast
        linfo.sparams = sparams
        linfo.inferred = infr
        linfo.module = mod
        linfo.roots = roots
        if !is(capt,nothing)
            linfo.capt = capt
        end
        known_lambda_data[lnumber] = linfo
    end
    return linfo
end

function deserialize_(s, ::Type{Array}, pos)
    d1 = deserialize(s)
    if isa(d1,Type)
        elty = d1
        d1 = deserialize(s)
    else
        elty = UInt8
    end
    if isa(d1,Integer)
        if elty !== Bool && isbits(elty)
            A = Array(elty, d1)
            deserialize_cycle(s, A, pos)
            return read!(s.io, A)
        end
        dims = (int(d1),)
    else
        dims = convert(Dims, d1)::Dims
    end
    if isbits(elty)
        n = prod(dims)::Int
        if elty === Bool && n>0
            A = Array(Bool, dims)
            i = 1
            while i <= n
                b = read(s.io, UInt8)
                v = bool(b>>7)
                count = b&0x7f
                nxt = i+count
                while i < nxt
                    A[i] = v; i+=1
                end
            end
        else
            A = read(s.io, elty, dims)
        end
        deserialize_cycle(s, A, pos)
        return A
    end
    A = Array(elty, dims)
    deserialize_cycle(s, A, pos)
    for i = 1:length(A)
        tag = int32(read(s.io, UInt8))
        if tag==0 || !is(deser_tag[tag], UndefRefTag)
            A[i] = handle_deserialize(s, tag)
        end
    end
    return A
end

deserialize_(s, ::Type{Expr}, p)     = deserialize_expr(s, int32(read(s.io, UInt8)), p)
deserialize_(s, ::Type{LongExpr}, p) = deserialize_expr(s, read(s.io, Int32), p)

function deserialize_expr(s, len, pos)
    hd = deserialize(s)::Symbol
    ty = deserialize(s)
    e = Expr(hd)
    deserialize_cycle(s, e, pos)
    e.args = Any[ deserialize(s) for i=1:len ]
    e.typ = ty
    e
end

function deserialize_(s, ::Type{UnionType}, p)
    types = deserialize(s)
    Union(types...)
end

function deserialize_(s, ::Type{DataType}, pos)
    form = read(s.io, UInt8)
    name = deserialize(s)::Symbol
    mod = deserialize(s)::Module
    ty = eval(mod,name)
    if ty.parameters === ()
        params = ()
    else
        params = deserialize(s)
    end
    if params === ()
        t = ty
    else
        t = apply_type(ty, params...)
    end
    if form == 0
        return t
    end
    if applicable(deserialize, s, t)
        return deserialize(s, t)
    end
    deserialize(s, t, pos)
end

deserialize_{T}(s, ::Type{Ptr{T}}, p) = convert(Ptr{T}, 0)

function deserialize_(s, ::Type{Task}, pos)
    t = Task(deserialize(s))
    deserialize_cycle(s, t, pos)
    t.storage = deserialize(s)
    t.state = deserialize(s)
    t.result = deserialize(s)
    t.exception = deserialize(s)
    t
end

# all other formats are handled by the default DataType deserializer
deserialize_(s, t::DataType, pos) = deserialize(s, t, pos)

# default DataType deserializer
function deserialize(s, t::DataType, pos)
    if length(t.names)==0 && t.size>0
        # bits type
        return read(s.io, t)
    end
    nf = length(t.names)
    if nf == 0
        return ccall(:jl_new_struct, Any, (Any,Any...), t)
    elseif isbits(t)
        if nf == 1
            return ccall(:jl_new_struct, Any, (Any,Any...), t, deserialize(s))
        elseif nf == 2
            f1 = deserialize(s)
            f2 = deserialize(s)
            return ccall(:jl_new_struct, Any, (Any,Any...), t, f1, f2)
        elseif nf == 3
            f1 = deserialize(s)
            f2 = deserialize(s)
            f3 = deserialize(s)
            return ccall(:jl_new_struct, Any, (Any,Any...), t, f1, f2, f3)
        else
            flds = Any[ deserialize(s) for i = 1:nf ]
            return ccall(:jl_new_structv, Any, (Any,Ptr{Void},UInt32), t, flds, nf)
        end
    else
        x = ccall(:jl_new_struct_uninit, Any, (Any,), t)
        deserialize_cycle(s, x, pos)
        for i in 1:length(t.names)
            tag = int32(read(s.io, UInt8))
            if tag==0 || !is(deser_tag[tag], UndefRefTag)
                ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), x, i-1, handle_deserialize(s, tag))
            end
        end
        return x
    end
end
