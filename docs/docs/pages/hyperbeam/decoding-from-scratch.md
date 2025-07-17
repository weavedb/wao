# Decoding HyperBEAM from Scratch

![](/images/decoding-from-sctatch.png)

Welcome to the complete guide to understanding HyperBEAM from the ground up.

## What This Tutorial Is For

This tutorial series takes you on a deep dive into HyperBEAM internals, teaching you everything from basic concepts to advanced implementations. You'll learn by doing - writing tests with WAO (HyperBEAM SDK in JS) that interact directly with HyperBEAM nodes.

### Why WAO + HyperBEAM?

WAO transforms the complex world of HyperBEAM development into something approachable:

- **JavaScript-First Development**: Everything is JavaScript - no need to context-switch between languages during testing
- **Sandbox Testing Environment**: WAO automatically spins up isolated HyperBEAM nodes for each test suite, ensuring clean, reproducible tests
- **Simplified APIs with Syntactic Sugar**: WAO's succinct API abstracts away complexity while giving you full control when needed
- **Automatic Codec & Signing Handling**: Complex encoding/decoding pipelines and HTTP message signatures are handled seamlessly in the background

### What You'll Master

By working through this tutorial series, you'll gain:

- **Deep Protocol Knowledge**: Understand how messages flow through HyperBEAM's codec pipeline, from structured encoding to HTTP signatures
- **Device Development Skills**: Build custom HyperBEAM devices that extend the platform's capabilities
- **Process Management Expertise**: Learn to spawn, schedule, and manage stateful computations
- **AOS Integration Know-How**: Connect HyperBEAM with the broader AO ecosystem
- **Payment System Understanding**: Implement complex payment flows using HyperBEAM's built-in devices

This isn't just theoretical knowledge - you'll have practical tools and patterns to build production-ready applications on HyperBEAM and interact seamlessly with AOS processes.

## The Journey Ahead

This series follows a carefully crafted path from basics to advanced topics. Each chapter builds on the previous ones, so it's important to follow them in order:

### **Part 1: Foundations**
| Chapter | Topic |
|---------|-------|
| 1 | **[Installing HyperBEAM and WAO](/hyperbeam/installing-hb-wao)** - Setting up your development environment |
| 2 | **[Devices and Pathing](/hyperbeam/devices-pathing)** - Understanding HyperBEAM's URL routing and device system |
| 3 | **[Custom Devices and Codecs](/hyperbeam/custom-devices-codecs)** - Building your first custom device and learning about codecs |

### **Part 2: The Codec System**
| Chapter | Topic |
|---------|-------|
| 4 | **[Flat Codec](/hyperbeam/codec-flat)** - Deep dive into path flattening for HTTP compatibility |
| 5 | **[Structured Codec](/hyperbeam/codec-structured)** - Handling complex data types with AO's type system |
| 6 | **[Httpsig Codec](/hyperbeam/codec-httpsig)** - Preparing messages for HTTP signatures |

### **Part 3: Security & Verification**
| Chapter | Topic |
|---------|-------|
| 7 | **[HTTP Message Signatures](/hyperbeam/http-message-signatures)** - Implementing RFC-9421 for message verification |
| 8 | **[Hashpaths](/hyperbeam/hashpaths)** - Understanding compute verification through chained hashes |

### **Part 4: Advanced Concepts**
| Chapter | Topic |
|---------|-------|
| 9 | **[Device Composition](/hyperbeam/device-composition)** - Combining devices for powerful workflows |
| 10 | **[Processes and Scheduler](/hyperbeam/processes-scheduler)** - Managing stateful computations |
| 11 | **[Legacynet Compatible AOS](/hyperbeam/legacynet-aos)** - Integrating with the AOS ecosystem |
| 12 | **[Payment System](/hyperbeam/payment-system)** - Building complex payment systems with p4@1.0 |

## Resources

- **[Working Test Suite](https://github.com/weavedb/wao/tree/master/dhfs-tutorial-app)** - Complete test files for all chapters
- **[HyperBEAM Implementation with Tutorial Devices](https://github.com/weavedb/HyperBEAM/tree/wao)** - HyperBEAM fork with all custom devices from this tutorial

---

:::note
*This tutorial series is a living document. As HyperBEAM evolves, so will these tutorials. If you find errors or have suggestions, please contribute to making this resource better for everyone.*
:::
