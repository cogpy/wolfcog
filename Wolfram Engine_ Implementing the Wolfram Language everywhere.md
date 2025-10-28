---
title: "Wolfram Engine: Implementing the Wolfram Language everywhere"
source: "https://www.wolfram.com/engine-technology/"
author:
published:
created: 2025-10-28
description: "Wolfram Engine software system powers Wolfram products and services and implements the Wolfram Language and its interfaces and connections across an unprecedentedly broad range of computational environments."
tags:
  - "clippings"
---
Implementing the Wolfram Language everywhere

The Wolfram Engine is the software system that powers Wolfram products and services, and implements the Wolfram Language and its interfaces and connections, across an unprecedentedly broad range of computational environments.

Under continuous development since 1986, the Wolfram Engine represents a remarkable software engineering achievement, using a host of different methods and technologies to adapt its delivery of sophisticated computation and knowledge to the full spectrum of desktop, cloud, mobile and embedded environments, and to support both human and machine usage.

## Versions & Environments

The Wolfram Engine is available for many environments to support Wolfram products and services.

  

![](https://www.wolfram.com/engine-technology/images/desktops.png)

##### Wolfram Engine for Desktop Systems

The classic Wolfram Engine, supporting both the notebook interactive interface and command-line operation.

Mac OS X, Windows, Linux, Linux ARM

![](https://www.wolfram.com/engine-technology/images/engine_servers.png)

##### Wolfram Engine for Servers

The Wolfram Engine operating in a persistent server mode, offering [Wolfram Symbolic Transfer Protocol (WSTP)](http://reference.wolfram.com/language/guide/WSTPAPI.html) and HTTP connectivity.

Linux and other Unix systems

  

![](https://www.wolfram.com/engine-technology/images/engine_cloud.png)

##### Wolfram Engine in the Cloud

The core system inside Wolfram's Cloud products and services, offering both interactive notebook and API-based access.

Wolfram Cloud, Wolfram Private Clouds

![](https://www.wolfram.com/engine-technology/images/engine_mobile.png)

##### Wolfram Engine for Mobile

Native computation and interface for mobile devices, including the complete [Wolfram Language](https://www.wolfram.com/language/) and SDK.

iOS, Android

  

![](https://www.wolfram.com/engine-technology/images/embedded.png)

##### Wolfram Engine for Embedded Systems

A standalone persistent Wolfram Engine discoverable through Wolfram Launch Manager, with WSTP.

ARM, Intel

![](https://www.wolfram.com/engine-technology/images/engine_library.png)

##### Wolfram Engine Library

A linkable shared library version of the Wolfram Engine, suitable for inclusion in deployed products.

To be available for various development environments

  

![](https://www.wolfram.com/engine-technology/images/engine_sdk.png)

##### Wolfram Engine SDK

A flexible software development kit suitable for OEM applications of the Wolfram Engine.

All supported platforms

![](https://www.wolfram.com/engine-technology/images/blank.jpg)

##### Also: [Wolfram High-Performance Computing System »](http://www.wolfram.com/solutions/hpc/)

### Custom Software Engineering Solutions

[

##### Wolfram Technology Integration System »

](https://www.wolfram.com/technology-integration/)

Development and deployment for flexible integration into your technology stack.

##### Wolfram Embedded Computing System

Development and deployment for embedded systems and devices.

  

## Components

The Wolfram Engine is a complex assembly of software components running locally and as distributed software.

##### Wolfram Language Kernel

The software component that implements the core Wolfram Language interpreter.

##### WSTP Connection Endpoints

The primary connection between Wolfram Engine software components, as well as closely coupled external systems.

##### Import/Export Framework

The framework for handling external formats in the Wolfram Engine.

##### External Services Framework

The framework for connecting to external APIs and related services.

##### Notebook Front End

The software component supporting interactive documents, separately implemented for each desktop and mobile operating system, and for the web.

##### Universal Deployment Endpoints

A variety of components supporting the [Wolfram Universal Deployment System](https://www.wolfram.com/universal-deployment-system).

##### Connected Devices Framework

The framework for local and distributed connectivity to external devices.

### Associated components:

##### Wolfram Knowledgebase

A distributed software component running in the Wolfram Cloud, providing knowledge for the Wolfram Engine.

##### Wolfram Launch Manager

A persistent software component that exposes a service for launching Wolfram Engine instances.

##### Wolfram License Manager

A system for managing Wolfram Engine usage in a local network.

##### Wolfram Linguistics System

A primarily cloud-based software component used to understand natural-language input.

##### Wolfram Grid Manager

A system for managing Wolfram Engine instances across a heterogeneous computing grid.

##### *WolframID* Manager

Global authentication system for distributed Wolfram services.

## Interface/Connection Technologies

The Wolfram Engine provides the core software to support the Wolfram Universal Deployment System.

##### Notebook Interface

The full interactive document-based interface for the Wolfram Language.

##### RESTful API

Wolfram Engine access through endpoints exposed by the [Instant API](http://reference.wolfram.com/language/guide/CreatingAnInstantAPI.html).

##### Wolfram Symbolic Transfer Protocol

The primary full-function high-level interprocess communication protocol used for Wolfram software components.

##### Command-Line Interface

Direct text-based command-line interface for the Wolfram Language.

##### Function Call Interface

Wolfram Engine access through embedded function calls from many languages.

##### Library Link

Load your own native shared libraries directly into the Wolfram Engine.

#### Of Note...

##### Three Decades of Development

Components of the Wolfram Engine have been in continuous development since 1986.

##### Tight Software Engineering

The Wolfram Engine benefits from Wolfram's longstanding emphasis on tight software engineering.

##### Broad Multi-platform Compatibility

The Wolfram Engine operates with full compatibility across a broad range of computational environments.

##### Over 30 million lines of code

Much of the Wolfram Engine is written in the Wolfram Language, making it perhaps ten times shorter than in other languages.

##### Frontier Hardware Support

The Wolfram Engine is routinely the first major software system ported to newly developed hardware systems.

##### Uniform Quality Assurance

Through its uniform software base, the Wolfram Engine can be efficiently tested for many different environments.

#### Related

![](https://www.wolfram.com/images/private_cloud.png)

##### Wolfram Private Cloud

Install a complete private instance of the Wolfram Cloud inside your organization.

[

![](https://www.wolfram.com/engine-technology/images/UDS.png)

##### WOLFRAM UNIVERSAL DEPLOYMENT SYSTEM »

Instant deployment across cloud, desktop, mobile and more.

](https://www.wolfram.com/universal-deployment-system/)

![](https://www.wolfram.com/images/cloud-app.png)

##### Wolfram Cloud App

Access programs deployed in the Wolfram Cloud directly from a mobile app.

[

![](https://www.wolfram.com/engine-technology/images/knowledgebase.png)

##### Wolfram Knowledgebase »

Curated computable knowledge powering Wolfram|Alpha.

](https://www.wolfram.com/knowledgebase)