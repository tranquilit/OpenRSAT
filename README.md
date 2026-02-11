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
With OpenRSAT, you can easely connect to an Active Directory with simple or kerberos connection, with TLS, with current account or even with a User/Password connection. You can also create different profile so you can switch connection easely.
Features are splitted in different modules:
- Users and computers: Manage users and machines within groups and containers to securly organize ressources in your AD. 
- DNS: Manage network entries in your AD.
- Sites and Services: Manage sites and services in your AD.
- Services and Interfaces: Overview of all objects in your AD.

## Users and computers
- Overview of containers hierarchy in a tree view.
- Overview of objects in a container in a grid view.
- Create/Edit/Delete object such as containers, groups, users, computers,...
- View and edit object's properties. You can see all attributs of an object, its security informations, LAPS, Bitlocker,...
- Search object in your active directory.
- Delegate control to others.

## DNS
/!\ The DNS module is using LDAP protocole instead of DCERPC. There is some different behavior, but you can manage DNS even if DNS with DCERPC is blocked.
- Overview of DNS zones in a tree view.
- Overview of DNS records in a grid view.
- Create/Delete DNS zones and DNS records.

## Sites and Services
- Overview of sites and subnets in a tree view.
- Overview of sites and subnets of a container in a grid view.
- Create/Delete sites and subnets.
- View and edit sites and subnets properties.
- Search sites and subnets.

# Screenshots
Overview of OpenRSAT:
![](./assets/Screenshots/OpenRSAT%20-%20Overview.gif "OpenRSAT overview")
Users and Computers of OpenRSAT:
![](./assets/Screenshots/OpenRSAT%20-%20ADUC.gif "OpenRSAT Users and Computers")

# Get started
## Requirement
To use the tool, you need an access to an Active Directory. You can refer to the [Samba Documentation by Tranquil IT](https://samba.tranquil.it/doc/fr/#) to deploy an Active Directory on a server.


## How to install
### WAPT
As the product is made by Tranquil IT, which provide WAPT to deploy software on machines in a domain, you can find the tool in a WAPT package in our store just [here](https://wapt.tranquil.it/store/fr/tis-openrsat).
### Binary
The binaries are packaged manually from the WAPT packages. \
You can download latest binary just [here](https://github.com/tranquilit/OpenRSAT/releases).
### Build from source
You can find all steps just here: [build.md](./build.md).

# Documentation
Not documented yet, but work is in progress.

# Contributing
Feel free to contribute to the project. As the project is a new project, no contributing rules are defined yet.
Feel free to provide issues on github to tell us any bug or what you want to see in this tool.