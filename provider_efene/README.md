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
        { provider_efene, ".*", {git, "git@host:user/provider_efene.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 provider_efene
    ===> Fetching provider_efene
    ===> Compiling provider_efene
    <Plugin Output>
