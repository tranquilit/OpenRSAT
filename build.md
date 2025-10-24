# Build OpenRSAT from source

Here is the complex way to get the application.

### Note
This part of the documentation might have some issue.
If you have any trouble with the documentation, you can create an issue on github just [here](https://github.com/tranquilit/OpenRSAT/issues/new).

### Components:
- **git**: A comamnd line tool to clone the repository.
- **fpcupdeluxe**: A GUI installer for fpc and lazarus. It is the easiest way to get FPC and Lazarus with the version you needs.
- **Fpc**: Free Pascal Compiler, to compile the pascal source code.
- **Lazarus**: A Free Pascal IDE that provides the LCL library with graphical components.
- **mORMot2**: A Free Pascal library with a variety of usefull components such as an Ldap Client.
- **7Zip**: A tool used to unpack mormot2 statics.

## Install git

Follow the instructions [here](https://git-scm.com/install).

## Install fpcupdeluxe
You can follow one of those direct link:

- Linux
    - i386: [fpcupdeluxe-i386-linux](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-i386-linux)
    - x64: [fpcupdeluxe-x86_64-linux](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-x86_64-linux)
    - arm64: [fpcupdeluxe-arm-linux](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-arm-linux)
    - arm32: [fpcupdeluxe-aarch64-linux](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-aarch64-linux)
- MacOS
    - intel: [fpcupdeluxe-x86_64-darwin-cocoa.zip](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-x86_64-darwin-cocoa.zip)
    - arm64: [fpcupdeluxe-aarch64-darwin-cocoa.zip](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-aarch64-darwin-cocoa.zip)
- Windows
    - x86: [fpcupdeluxe-i386-win32.exe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-i386-win32.exe)
    - x64: [fpcupdeluxe-i386-win64.exe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/download/fpcupdeluxe-x86_64-win64.exe)

You can find the latest release just [here](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest/).

## Install 7Zip

Follow the instructions [here](https://7-zip.org/download.html).

## Install FPC and Lazarus

Open fpcupdeluxe. \
A popup should appear with this message:

![A popup to confirm you want to send log.](./assets/Screenshots/fpcupdeluxe-logs-confirmation.png)

You should click on **No**.

Then, you will find a similar page:

![fpcupdeluxe installation page](./assets/Screenshots/fpcupdeluxe-installation-page.png)

1. Click on **Set install path** and select a folder where you want to install fpc and lazarus. I recommand you to create a new folder where to install it. For this example, I assume you installed it in **/lazarus**.

2. Select the correct fpc's version: **fixes-3.2**.

3. Select the correct lazarus' version: **fixes-4.0**.

4. Click on **Install/update FPC+Lazarus** to launch the installation process.

Once finished, you can close the application.

## Clone repository

Assuming you correctly install **git** and set it to your path, here is the way to clone the repository:

```bash
git clone --depth=1 https://github.com/tranquilit/OpenRSAT
git submodule update --init
```

For this example, I assume you clone the repository in **/OpenRSAT**

## Install mORMot2 statics

The mORMot2 library requires some external statics files. You must download them and install them properly to compile this project.

1. Download the archive with all the needs just here: https://github.com/synopse/mORMot2/releases/latest/download/mormot2static.7z .

2. Unpack the archive with 7Zip. Here is a command line that do it:

```bash
7zz x -y mormot2static.7z -ostatic
```

3. Move the extracted folder to: **OpenRSAT/submodules/mORMot2/static**.

# Configure Lazarus

To configure lazarus to build the project, you must configure it with some lazarus packages. Here is the command you should run to do so:

```bash
/lazarus/lazarus/lazbuild --add-package-link /OpenRSAT/submodules/mORMot2/packages/lazarus/mormot2.lpk
/lazarus/lazarus/lazbuild --add-package-link /OpenRSAT/submodules/pltis_uicomponents/pack/pltis_uicomponents.lpk
/lazarus/lazarus/lazbuild --add-package-link /OpenRSAT/submodules/pltis_utils/pltis_utils.lpk
/lazarus/lazarus/lazbuild --add-package /OpenRSAT/submodules/pltis_virtualtrees/virtualtreeview_package.lpk
/lazarus/lazarus/lazbuild --add-package /OpenRSAT/submodules/metadarkstyle/metadarkstyle.lpk
/lazarus/lazarus/lazbuild --add-package /OpenRSAT/submodules/metadarkstyle/metadarkstyledsgn.lpk
/lazarus/lazarus/lazbuild --add-package-link /OpenRSAT/packages/OpenRSATCore/OpenRSATCore.lpk
```

## Build project

If everything has been install correctly, this is the easiest part.

- Linux
    - i386: 
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=linux-i386 /OpenRSAT/sources/OpenRSAT.lpi
        ```
    - x64:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=linux-x64 /OpenRSAT/sources/OpenRSAT.lpi
        ```
    - arm64:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=linux-arm64 /OpenRSAT/sources/OpenRSAT.lpi
        ```
    - arm32:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=linux-arm32 /OpenRSAT/sources/OpenRSAT.lpi
        ```
- MacOS
    - intel:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=macosx-intel /OpenRSAT/sources/OpenRSAT.lpi
        ```
    - arm64:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=macosx-arm64 /OpenRSAT/sources/OpenRSAT.lpi
        ```
- Windows
    - x86: 
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=win32 /OpenRSAT/sources/OpenRSAT.lpi
        ```
    - x64:
        ```bash
        /lazarus/lazarus/lazbuild --build-mode=win64 /OpenRSAT/sources/OpenRSAT.lpi
        ```

