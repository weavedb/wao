---
name: build-device
description: "Write HyperBEAM Erlang device with eunit tests. Iterates until compilation and tests pass 100%. Use when user says 'build the device', 'write the Erlang code', or 'implement the device'. Not for scaffolding — use /create-device."
argument-hint: "<task-id>"
disable-model-invocation: true
allowed-tools: Read, Write, Edit, Bash, Grep, Glob
---

Build a HyperBEAM Erlang device and/or eunit tests for a task. Iterates until compilation and all tests pass 100%.

## Resolve HyperBEAM path

The HyperBEAM location is configured in `.env.hyperbeam` via the `CWD` variable. This is set during `npx wao create` (clone, link, or skip).

Read `.env.hyperbeam` and extract the `CWD` value. Store it as `$HB_DIR` for all subsequent steps.

```bash
grep '^CWD=' .env.hyperbeam 2>/dev/null | cut -d= -f2-
```

If `.env.hyperbeam` doesn't exist or has no `CWD`, **STOP immediately** and tell the user:

> No HyperBEAM configured. Device tasks require a HyperBEAM installation.
>
> Set it up by adding a `CWD` entry to `.env.hyperbeam`:
> ```bash
> echo "CWD=/path/to/your/HyperBEAM" >> .env.hyperbeam
> ```
>
> Or re-run `npx wao create` and choose option 1 (clone) or 2 (link).

Then verify the directory exists and has source:

```bash
test -d "$HB_DIR/src" && echo "OK" || echo "MISSING"
```

If `MISSING`, stop and tell the user the path in `.env.hyperbeam` is invalid.

Also verify rebar3 is available:

```bash
which rebar3 && echo "OK" || echo "MISSING"
```

If rebar3 is missing, stop and tell the user to install Erlang/OTP 27+ and rebar3.

Do NOT mark the task as done if any prerequisite fails. Set the task status to `"pending"` and stop.

## Steps

1. Read `tasks.json` and find the task matching `$ARGUMENTS` (task id). Update its status to `"in_progress"`.

2. Read `plan.md` for device specifications:
   - Device name and purpose
   - Exported functions (actions)
   - State management
   - Auth requirements

3. Read `docs/hyperbeam-dev.md` for the device protocol (arity/3 functions, state management, auth patterns).

4. Write the Erlang device with **inline eunit tests** (HyperBEAM convention — device and tests live in the same file):

   Write `$HB_DIR/src/dev_{name}.erl`:

   ```erlang
   -module(dev_{name}).
   -export([info/3, init/3, compute/3]).
   -include("include/hb.hrl").
   -ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
   -endif.

   %% Exports: list available keys
   info(_Msg1, _Msg2, Opts) ->
       {ok, #{
           <<"exports">> => [<<"init">>, <<"compute">>]
       }}.

   %% Initialize device state
   init(Msg1, _Msg2, Opts) ->
       {ok, hb_maps:put(<<"initialized">>, true, Msg1)}.

   %% Main compute — route by Action tag
   compute(Msg1, Msg2, Opts) ->
       Action = hb_ao:get(<<"Action">>, Msg2, Opts),
       handle_action(Action, Msg1, Msg2, Opts).

   handle_action(<<"MyAction">>, Msg1, Msg2, Opts) ->
       %% Implementation here
       {ok, Msg1};
   handle_action(_, Msg1, _Msg2, _Opts) ->
       {ok, hb_maps:put(<<"Error">>, <<"Unknown action">>, Msg1)}.

   %%%===================================================================
   %%% EUnit Tests (inline — HyperBEAM convention)
   %%%===================================================================
   -ifdef(TEST).

   info_test() ->
       {ok, Info} = info(#{}, #{}, #{}),
       ?assertMatch(#{<<"exports">> := _}, Info).

   compute_test() ->
       {ok, State} = init(#{}, #{}, #{}),
       Msg = #{<<"Action">> => <<"MyAction">>},
       {ok, Result} = compute(State, Msg, #{}),
       ?assertMatch(#{}, Result).

   -endif.
   ```

   Key patterns:
   - **Device + tests in same file** — use `-ifdef(TEST).` / `-endif.` guards
   - Tests call local functions directly (no `module:function` prefix needed)
   - Use `hb_maps` for state (not plain maps)
   - Use `hb_private` for secrets
   - Use `ar_wallet` for auth/signing
   - Route actions via `hb_ao:get(<<"Action">>, Msg2, Opts)`
   - Return `{ok, UpdatedMsg}` or `{error, Reason}`

   **Do NOT create separate test files** in `$HB_DIR/test/`. All eunit tests go inline in the device source file.

5. Compile (use the `$HB_DIR` path):

   ```bash
   cd $HB_DIR && rebar3 as genesis_wasm compile
   ```

   **Verify the .beam file was actually created:**

   ```bash
   ls $HB_DIR/_build/genesis_wasm/lib/hb/ebin/dev_{name}.beam && echo "COMPILED" || echo "NOT FOUND"
   ```

   If the beam file is missing, compilation failed silently. Read the full output and fix.

6. Run eunit tests (note: test the device module directly, not a separate test module):

   ```bash
   cd $HB_DIR && rebar3 eunit --module=dev_{name}
   ```

8. If compilation errors or test failures:
   - Read the error output
   - Fix the Erlang code
   - Recompile and re-test
   - Repeat until **100% pass**

9. **Register the device atom** so HyperBEAM can resolve the device name at runtime.

   The device name (e.g., `"dev_{name}@1.0"`) must be pre-registered as an atom in the HyperBEAM startup eval. Check if it already exists in `src/hyperbeam.js` (in the `preRegisterAtoms` section). If not, inform the user to add it. This is only needed for WAO SDK integration tests — eunit tests don't require it.

10. Update the task status to `"done"` in `tasks.json`.

    **IMPORTANT**: Only mark done if:
    - The `.erl` source file exists in `$HB_DIR/src/`
    - The `.beam` file exists in `$HB_DIR/_build/`
    - All eunit tests pass

## Troubleshooting

### rebar3 compile: "module not found"
- Verify `-module(dev_{name}).` matches the filename
- Ensure file is in `$HB_DIR/src/`, not a subdirectory

### eunit hangs
- Check for `hb_cache` calls that may block without store config
- Use `#{}` as Opts in eunit tests, not production Opts

### Device not found at runtime
- Device atom must be pre-registered — check `preloaded_devices`
- Verify module name matches `dev_{name}` convention

## Iteration Protocol

For each test-fix cycle:
1. Run tests and capture FULL output
2. Identify the FIRST failing test (fix failures in order)
3. Classify: is the bug in source code or test code?
4. Fix ONE issue per iteration (don't change multiple things at once)
5. Re-run only the failing test first, then full suite
6. If the same error persists after 3 different fix attempts, step back:
   - Re-read the relevant docs (HyperBEAM device protocol, Erlang patterns, etc.)
   - Check if the plan itself has a wrong assumption
   - Try a completely different approach
7. Continue iterating until all tests pass — there is no retry limit

Only escalate to the user if:
- The failure is environmental (rebar3 missing, Erlang version, port conflict)
- The test requires user input (wallet, external service URL)
- You've tried 10+ iterations with no progress on the same error
