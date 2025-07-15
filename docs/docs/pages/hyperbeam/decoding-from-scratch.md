# Introduction - Decoding HyperBEAM from Scratch

![](/images/decoding-from-sctatch.png)

:::note
Welcome to the complete guide to understanding AO Core and HyperBEAM from the ground up.

If you've ever wondered how the Arweave compute layer actually works under the hood, or felt overwhelmed by the complexity of decentralized computing protocols, this tutorial series is for you.
:::

---

## What You'll Learn

This isn't just another "hello world" tutorial. We're going to systematically decode every aspect of HyperBEAM by building, breaking, and rebuilding its components. 

| **Topic** | **What You'll Master** |
|-----------|------------------------|
| **Device Architecture** | How HyperBEAM's modular device system works and how to build your own custom devices |
| **Message Encoding/Decoding** | The complete journey of a message through structured encoding, flat encoding, HTTP signatures, and back |
| **Hashpaths & Verifiability** | How compute steps are chained and made cryptographically verifiable |
| **Process Management** | Spawning processes, scheduling messages, and managing state transitions |
| **Advanced Patterns** | Device composition, payment systems, and AOS integration |

---

## Why This Matters

:::warning
HyperBEAM isn't just another blockchain project - it's a fundamental reimagining of how computation can be distributed, verified, and composed.
:::

Understanding its internals gives you the power to:

1. **Build Better Applications**  
   When you understand the protocol, you can design more efficient and powerful applications

2. **Debug with Confidence**  
   No more black box mysteries - you'll know exactly what's happening at every step

3. **Contribute to the Ecosystem**  
   The AO ecosystem needs developers who deeply understand the technology

4. **Push Boundaries**  
   With deep knowledge comes the ability to innovate and extend the platform in ways others can't imagine

---

## Our Approach

### **Hands-On, Test-Driven Learning**

We'll use a hands-on, test-driven approach throughout this series. Every concept will be explored through actual code you can run and modify. We'll start with the basics and gradually build up to complex systems like payment processors and AOS modules.

### **Tools We'll Use**

| Tool | Purpose |
|------|---------|
| **HyperBEAM** | A local node running on your machine |
| **WAO** | A testing framework and SDK that makes interacting with HyperBEAM intuitive |
| **JavaScript/Node.js** | For writing tests and exploring the system |
| **Erlang** | For building custom HyperBEAM devices (minimal usage) |

:::tip
Don't worry if you're not familiar with Erlang - we'll introduce concepts as needed, and most of your interaction will be through JavaScript.
:::

---

## The Journey Ahead

This series follows a carefully crafted path from basics to advanced topics. Each chapter builds on the previous ones, so it's important to follow them in order:

### **Part 1: Foundations**
| Chapter | Topic | What You'll Learn |
|---------|-------|-------------------|
| 1 | **Installing HyperBEAM and WAO** | Setting up your development environment |
| 2 | **Devices and Pathing** | Understanding HyperBEAM's URL routing and device system |
| 3 | **Custom Devices and Codecs** | Building your first custom device and learning about codecs |

### **Part 2: The Codec System**
| Chapter | Topic | What You'll Learn |
|---------|-------|-------------------|
| 4 | **Flat Codec** | Deep dive into path flattening for HTTP compatibility |
| 5 | **Structured Codec** | Handling complex data types with AO's type system |
| 6 | **Httpsig Codec** | Preparing messages for HTTP signatures |

### **Part 3: Security & Verification**
| Chapter | Topic | What You'll Learn |
|---------|-------|-------------------|
| 7 | **HTTP Message Signatures** | Implementing RFC-9421 for message verification |
| 8 | **Hashpaths** | Understanding compute verification through chained hashes |

### **Part 4: Advanced Concepts**
| Chapter | Topic | What You'll Learn |
|---------|-------|-------------------|
| 9 | **Device Composition** | Combining devices for powerful workflows |
| 10 | **Processes and Scheduler** | Managing stateful computations |
| 11 | **Legacynet Compatible AOS** | Integrating with the AOS ecosystem |
| 12 | **Payment System** | Building complex systems with faff@1.0 and p4@1.0 |

---

## Prerequisites

To get the most out of this series, you should have:

- **Basic JavaScript Knowledge**  
  You should be comfortable with async/await, ES6 modules, and Node.js
  
- **Command Line Familiarity**  
  We'll be running commands and managing processes
  
- **Curiosity**  
  The most important prerequisite - a desire to understand how things really work

:::note
You don't need to be an expert in blockchain, cryptography, or distributed systems. We'll explain these concepts as we encounter them.
:::

---

## A Different Kind of Tutorial

:::danger
**Most tutorials show you how to _use_ a technology.**  
**This series shows you how the technology _works_.**

This means we'll sometimes write code that the SDK normally handles for you. We'll manually encode messages, construct signatures, and manage hashpaths. Why? Because true understanding comes from doing things the hard way first.
:::

---

## Getting Started

Ready to begin? The next chapter will walk you through setting up HyperBEAM and WAO on your local machine. From there, we'll start our exploration with simple device interactions and gradually work our way up to building complex, production-ready systems.

:::tip
**Remember**: Every expert was once a beginner who refused to give up. The concepts might seem overwhelming at first, but stick with it. By the end of this series, you'll have a deep, practical understanding of one of the most innovative compute platforms in the decentralized web.
:::

**Let's decode HyperBEAM together.**

---

## Learning Steps

1. [Installing HyperBEAM and WAO](./installing-hb-wao.md)
2. [Devices and Pathing](./devices-pathing.md)
3. [Custom Devices and Codecs](./custom-devices-codecs.md)
4. [Flat Codec](./codec-flat.md)
5. [Structured Codec](./codec-structured.md)
6. [Httpsig Codec](./codec-httpsig.md)
7. [HTTP Message Signatures](./http-message-signatures.md)
8. [Hashpaths](./hashpaths.md)
9. [Device Composition](./device-composition.md)
10. [Processes and Scheduler](./processes-scheduler.md)
11. [Legacynet Compatible AOS](./legacynet-aos.md)
12. [Payment System](./payment-system.md)

---

:::note
*This tutorial series is a living document. As HyperBEAM evolves, so will these tutorials. If you find errors or have suggestions, please contribute to making this resource better for everyone.*
:::
