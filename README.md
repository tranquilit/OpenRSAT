# OpenRSAT (Open Remote System Administration Tools)
A new console that look like Microsoft RSAT written in modern Object Pascal.

# Overview
This repository provide a Lazarus project with all sources and dependances to build the OpenRSAT console.
The console is available on different platforms such as Windows, Linux, and Macos.
The OpenRSAT is a tool that allow you to connect to your Active Directory, and manage it as you wish.

The objective is to allow system administrators to manage an Active Directory on a Linux or Macos platform.
Associated with Samba AD, you'll be able to fully manage an Active Directory for free with open source solutions.
A secondary objective is to provide new feature that are missing in Microsoft RSAT.

# Features
Features are splitted in different modules:
- Users and computers: Manage users and machines within groups and containers to securly organize ressources in your AD. 
- DNS: Manage network entries in your AD.
- Sites and Services: Manage sites and services in your AD.
- Services and Interfaces: Overview of all objects in your AD.

# Screenshots
Overview of OpenRSAT:
![](./assets/Screenshots/OpenRSAT%20-%20Overview.gif "OpenRSAT overview")
Users and Computers of OpenRSAT:
![](./assets/Screenshots/OpenRSAT%20-%20ADUC.gif "OpenRSAT Users and Computers")
No screenshots yet, but work is in progress.

# Get started
## Requirement
To use the tool, you need an access to an Active Directory. You can refer to the [Samba Documentation by Tranquil IT](https://samba.tranquil.it/doc/fr/#) to deploy an Active Directory on a server.


## How to install
### WAPT
As the product is made by Tranquil IT, which provide WAPT to deploy software on machines in a domain, you can find the tool in a WAPT package in our store just [here](https://wapt.tranquil.it/store/fr/tis-openrsat).
### Binary
The binaries are packaged manually from the WAPT packages. \
/!\\ On MacOS, you should run this command to remove the app from the quarantine: `xattr -dr com.apple.quarantine /Applications/OpenRSAT.app`. The reason is that the application has not yet been signed or notarized, either by Apple or Tranquil IT. \
We'll also provide a workflow on github that will build new release.
### Build from source
We'll provide a step by step explaination to build the product. It requires FPC (FreePascalCompiler) and Lazarus.
You can easly install fpc and lazarus from [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe).

# Documentation
Not documented yet, but work is in progress.

# Contributing
Feel free to contribute to the project. As the project is a new project, no contributing rules are defined yet.
Feel free to provide issues on github to tell us any bug or what you want to see in this tool.