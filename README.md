# OpenRSAT (Open Remote System Administration Tools)

A cross-platform console inspired by Microsoft RSAT, written in modern Object Pascal (Lazarus).

![License](https://img.shields.io/github/license/tranquilit/OpenRSAT)
![Latest release](https://img.shields.io/github/v/release/tranquilit/OpenRSAT)
![Stars](https://img.shields.io/github/stars/tranquilit/OpenRSAT)
![Platforms](https://img.shields.io/badge/platforms-Windows%20%7C%20Linux%20%7C%20macOS-blue)

## Table of contents

* [Overview](#overview)
* [Features](#features)
* [Screenshots](#screenshots)
* [Get started](#get-started)
* [Translations](#translations)
* [Documentation](#documentation)
* [Community and support](#community-and-support)
* [Contributing](#contributing)
* [License](#license)

## Overview

This repository provides a Lazarus project with all the sources and dependencies needed to build the OpenRSAT console.
The console runs on several platforms: Windows, Linux, and macOS.
OpenRSAT is a tool that lets you connect to your Active Directory and manage it the way you want.

The main objective is to let system administrators manage an Active Directory from a Linux or macOS workstation.
Paired with a Samba AD, you can fully manage an Active Directory for free, with open source solutions only.
A secondary objective is to provide new features that are missing in Microsoft RSAT.

## Features

With OpenRSAT you can connect to an Active Directory using a simple or a Kerberos connection, with TLS, with the current account or with a user/password connection. You can also create several profiles to switch between connections easily.

Features are split into different modules:

* **Users and Computers**: manage users and machines within groups and containers to organize the resources of your AD securely.
* **DNS**: manage the network entries of your AD.
* **Sites and Services**: manage the sites and services of your AD.
* **Services and Interfaces**: get an overview of every object in your AD.

### Users and Computers

* Overview of the container hierarchy in a tree view.
* Overview of the objects of a container in a grid view.
* Create / edit / delete objects such as containers, groups, users, computers, and more.
* View and edit an object's properties. You can see all the attributes of an object, its security information, LAPS, BitLocker, and more.
* Search for an object in your Active Directory.
* Delegate control to others.

### DNS

> [!WARNING]
> The DNS module uses the LDAP protocol instead of DCERPC. The behavior differs slightly, but it lets you manage DNS even when DNS over DCERPC is blocked.

* Overview of the DNS zones in a tree view.
* Overview of the DNS records in a grid view.
* Create / delete DNS zones and DNS records.

### Sites and Services

* Overview of the sites and subnets in a tree view.
* Overview of the sites and subnets of a container in a grid view.
* Create / delete sites and subnets.
* View and edit the properties of sites and subnets.
* Search for sites and subnets.

### Services and Interfaces

* Overview of every object in your AD, useful to inspect the raw content of the directory.

## Screenshots

Overview of OpenRSAT:

[![OpenRSAT overview](https://github.com/tranquilit/OpenRSAT/raw/main/assets/Screenshots/OpenRSAT%20-%20Overview.gif "OpenRSAT overview")](/tranquilit/OpenRSAT/blob/main/assets/Screenshots/OpenRSAT%20-%20Overview.gif)

Overview of properties:

[![OpenRSAT overview properties](https://github.com/tranquilit/OpenRSAT/raw/main/assets/Screenshots/OpenRSAT%20-%20Overview%20properties.gif "OpenRSAT overview properties")](/tranquilit/OpenRSAT/blob/main/assets/Screenshots/OpenRSAT%20-%20Overview%20properties.gif)

Create user:
[![OpenRSAT create user](https://github.com/tranquilit/OpenRSAT/raw/main/assets/Screenshots/OpenRSAT%20-%20Create%20user.gif "OpenRSAT create user")](/tranquilit/OpenRSAT/blob/main/assets/Screenshots/OpenRSAT%20-%20Create%20user.gif)

## Get started

### Requirements

To use the tool, you need access to an Active Directory. You can refer to the [Samba documentation by Tranquil IT](https://samba.tranquil.it/doc/fr/) to deploy an Active Directory on a server.

### How to install

#### WAPT

OpenRSAT is made by Tranquil IT, which also provides WAPT to deploy software on the machines of a domain. You can find the tool as a WAPT package in our store, just [here](https://wapt.tranquil.it/store/fr/tis-openrsat).

#### Binary

The binaries are packaged manually from the WAPT packages.
You can download the latest binary just [here](https://github.com/tranquilit/OpenRSAT/releases).

#### Build from source

You will find all the steps just here: [build.md](https://github.com/tranquilit/OpenRSAT/blob/main/build.md).

## Translations

OpenRSAT is built by a French company, so the tool ships with English and French. A Greek translation is also available, contributed by [@parapente](https://github.com/parapente).
Don't hesitate to ask for more languages, or to contribute one yourself.

## Documentation

Not documented yet.

## Community and support

* [Issues](https://github.com/tranquilit/OpenRSAT/issues): report a bug or request a feature.
* [Discussions](https://github.com/tranquilit/OpenRSAT/discussions): ask questions, follow releases, and talk with other users.

## Contributing

Feel free to contribute to the project. As this is still a young project, no contributing rules are defined yet.
Feel free to open issues on GitHub to tell us about any bug, or about anything you would like to see in this tool.

## License

OpenRSAT is released under the [GNU General Public License v3.0](https://github.com/tranquilit/OpenRSAT/blob/main/LICENSE).