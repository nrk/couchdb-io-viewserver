JSON

funs       := List clone
mapResults := List clone

output  := method(data, File standardOutput writeln(data asJSON))
error   := method(id, message, 
    Map with("error", Map with("id", id, "reason", message))
)

// ----------------------------------------------------------------------------

log     := method(message, output(Map with("log", message)))
emit    := method(key, value, mapResults append(list(key, value)))
sum     := method(values, values reduce(+))

raise      := method(message, Exception raise(message))
raiseFatal := method(raise("fatal_error"))

// ----------------------------------------------------------------------------

compile := method(funSource,
    compiledCode := Message fromString(funSource unescape)

    (compiledCode doInContext type == "Block") ifTrue(
        return compiledCode
    ) ifFalse(
        return error(
            "compilation_error", 
            "expression does not eval to a function #{funSource unescape}" interpolate
        )
    )
)

execute := method(fun, 
    fun doInContext performWithArgList("call", call evalArgs slice(1))
)

handleCommand := method(cmd, 
    handlers hasKey(cmd at(0)) ifTrue(
        return handlers at(cmd at(0)) performWithArgList("call", cmd slice(1))
    ) ifFalse(
        return error("query_server_error", "unknown command #{cmd at(0)}" interpolate)
    )
)

// ----------------------------------------------------------------------------

handlers := Map with(
    "reset", method(
        funs = list()
        true
    ),

    "add_fun", method(fun_source, 
        ex := try(
            compiledCode := compile(fun_source)
        )
        ex catch(Exception, 
            error("compilation_error", ex error asMutable strip)
        )


        funs append(compiledCode)
        true
    ),

    "map_doc", method(doc, 
        result := list()

        funs foreach(fun, 
            ex := try(
                execute(fun, doc)
                result append(mapResults)
            )
            ex catch(Exception, 
                (ex error == "fatal_error") ifTrue(
                    self return error("map_runtime_error", "function raised fatal exception")
                ) ifFalse(
                    log("function raised exception (#{ex error}) with doc._id #{doc at(\"_id\")}" interpolate)
                    result removeAll
                )
            )
        )

        result
    ), 

    "reduce", method(reduceFuns, arguments, rereduce, 
        keys       := list()
        values     := list()
        reductions := list()

        (rereduce == true) ifFalse(
            arguments foreach(kv, 
                keys append(kv first)
                values append(kv second)
            )
        ) ifTrue(
            keys   = nil
            values = arguments
        )

        reduceFuns foreach(reduceFunSrc, 
            ex := try(
                result := execute(compile(reduceFunSrc), keys, values, rereduce)
                reductions append(result)
            )
            ex catch(Exception, 
                (ex error != "fatal_error") ifTrue(
                    log("function raised exception (#{ex error})" interpolate)
                    result removeAll
                ) ifFalse(
                    return error("reduce_runtime_error", "function raised fatal exception")
                )
            )
        )

        return list(true, reductions)
    ),

    "rereduce", method(reduceFuns, arguments, reduce(reduceFuns, arguments, true))
)

// ----------------------------------------------------------------------------

mainLoop := method(
    while (lineFromStdin := File standardInput readLine, 
        ex := try(
            command := JSON fromJSON(lineFromStdin)
        )
        ex catch(Exception,
            output(error('unknown_error', ex error))
        )

        output(handleCommand(command))
    )
)

mainLoop

