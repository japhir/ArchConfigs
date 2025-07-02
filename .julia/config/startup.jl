try
    using Revise
catch e
    @warn "Error initializing Revise"
end

if pwd() != "/home/japhir"
    try
        using Pkg
        Pkg.activate(".")
    catch e
        @warn "Error activating package"
    end
end

if haskey(ENV, "INSIDE_EMACS")
    ENV["JULIA_EDITOR"] = "emacsclient"
    ENV["DISPLAY"] = ":0"
else
    # only use OhMyREPL when not inside emacs
    # I want emacs to handle syntax highlighting and parentheses etc.
    try
        using OhMyREPL
    catch e
        @warn "Error initializing OhMyREPL"
    end
end
