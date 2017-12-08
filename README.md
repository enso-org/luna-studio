# Luna Studio

## Requirements

TODO: GHC reqs

## Building
  1. Download [luna-manager](http://luna-lang.org/luna-manager) or [build it from source](https://github.com/luna/luna-manager)
  2. Run `luna-manager develop luna-studio`, which will create a directory at `~/luna-develop`. You can optionally pass `--path PATH` command line argument to manually specify the destination.  
  3. For your convenience export following environment variables:
     * `export LUNA_DEV=~/luna-develop`
     * `export LUNA_STUDIO_DEV=$LUNA_DEV/apps/luna-studio`
  4. Change your working directory to `cd $LUNA_STUDIO_DEV`
  5. To build `luna-studio` from sources call `./build`. For further usage information call `./build --help`.
  6. After successful build you can test your new `luna-studio` version at `./dist/bin/main/luna-studio --develop`. Use `--help` for available running options.

## Build environment
  * Your luna-studio build environment `$LUNA_STUDIO_DEV` is a `git` repository and you can use it just as a regular source code repository.
  * Note that using `git clean -x` will remove the `$LUNA_STUDIO_DEV/dist` directory content, containing dependencies needed for building `luna-studio`, which were downloaded there by `luna-manager`. To setup them again again call `luna-manager develop luna-studio --path $LUNA_STUDIO_DEV --download-dependencies`.
  
## Troubleshooting
  * If this problem occurs during build:
    ```text
    Initializing APM in: PATH_TO_LUNA_STUDIO/dist/user-config/atom/packages/luna-studio
    Traceback (most recent call last):
      File "./build", line 66, in <module>
        main()
      File "./build", line 64, in main
        else: build_app (args.stack_backend_args, args.stack_frontend_args, args.stack_runner_args, not args.release)
      File "./build", line 21, in build_app
        atom_apm.run(dev_mode) # RENAME: LunaStudioInstallInAtom ?
      File "PATH_TO_LUNA_STUDIO/scripts_build/atom_apm.py", line 162, in run
        init_apm(link)
      File "PATH_TO_LUNA_STUDIO/scripts_build/atom_apm.py", line 124, in init_apm
        output = run_apm('install', '.')
      File "PATH_TO_LUNA_STUDIO/scripts_build/atom_apm.py", line 87, in run_apm
        return run_process(apm_path, command, *args)
      File "PATH_TO_LUNA_STUDIO/scripts_build/atom_apm.py", line 75, in run_process
        proc = subprocess.Popen(pargs, stdout=subprocess.PIPE)
      File "/usr/local/Cellar/python3/3.6.1/Frameworks/Python.framework/Versions/3.6/lib/python3.6/subprocess.py", line 707, in __init__
        restore_signals, start_new_session)
      File "/usr/local/Cellar/python3/3.6.1/Frameworks/Python.framework/Versions/3.6/lib/python3.6/subprocess.py", line 1326, in _execute_child
        raise child_exception_type(errno_num, err_msg)
    FileNotFoundError: [Errno 2] No such file or directory: 'PATH_TO_LUNA_STUDIO/dist/third-party/Atom.app/Contents/Resources/app/apm/bin/apm'
    ```
    Please do: `luna-manager develop luna-studio --path $LUNA_STUDIO_DEV --download-dependencies`.
    
* [Linux] App image requires FUSE to run. While most Linux distributions have it pre-installed, it can be missing in your specific case. If your distro happens to lack FUSE, you can either install it or mount the AppImage yourself with:
    ```
    sudo mount -o loop Some.AppImage /mnt
    /mnt/AppRun
    ```
