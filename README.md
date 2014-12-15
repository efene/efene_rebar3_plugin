provider_efene
=====

efene rebar3 plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {efene_rebar3_plugin, ".*", {git, "git@github.com:efene/efene_rebar3_plugin.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efene
    ===> Fetching efene
    ===> Compiling efene
    <Plugin Output>
