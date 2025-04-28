// Core Types
export interface JWK {
    kty: string;
    e: string;
    n: string;
    d?: string;
    p?: string;
    q?: string;
    dp?: string;
    dq?: string;
    qi?: string;
}

export interface Tag {
    name: string;
    value: string;
}

export interface Message {
    process: string;
    tags: Tag[];
    data?: string | Uint8Array;
    signer?: JWK;
}

export interface ProcessOptions {
    boot?: boolean;
    module?: string;
    scheduler?: string;
    memory?: number;
    jwk?: JWK;
    tags?: Record<string, string>;
    data?: string | Uint8Array;
    auth?: string;
}

export interface ResultOptions {
    jwk?: JWK;
    pid?: string;
    mid?: string;
    check?: any[];
    get?: boolean | string;
    timeout?: number;
    mode?: 'aoconnect' | 'local';
    limit?: number;
}

export interface DeployOptions {
    boot?: boolean;
    loads?: any[];
    src?: string;
    src_data?: string;
    fills?: Record<string, any>;
    module?: string;
    scheduler?: string;
    jwk?: JWK;
    tags?: Record<string, string>;
    data?: string | Uint8Array;
}

// AO Class Types
export interface AOOptions {
    module?: string;
    scheduler?: string;
    memory?: number;
    ar?: {
        host?: string;
        port?: number;
        protocol?: string;
    };
}

export interface PipeOptions {
    jwk: JWK;
    fns: Function[];
    cb?: Function;
}

// WeaveDB Types
export interface WeaveDBOptions {
    cacheSize?: number;
    chunkSize?: number;
    notifySize?: number;
}

export interface WeaveDBCollection {
    create: (id: string) => Promise<void>;
    createData: (col: string, doc: any, val: any) => Promise<void>;
    open: (filename: string, val: any) => Promise<void>;
    read: (fd: number, raw_dst_ptr: number, raw_length: number, val: any) => Promise<void>;
    close: (fd: number) => void;
}

// WeaveDrive Types
export interface WeaveDriveOptions {
    cacheSize?: number;
    chunkSize?: number;
    streamChunk?: number;
}

export interface WeaveDriveFile {
    reset: (fd: number) => void;
    create: (id: string) => Promise<number>;
    read: (fd: number, raw_dst_ptr: number, raw_length: number) => Promise<number>;
    close: (fd: number) => void;
}

// Process Class Types
export interface ProcessOptions {
    pid: string;
    ao: AO;
}

// AR Types
export interface AROptions {
    host?: string;
    port?: number;
    protocol?: string;
}

export interface ARTransaction {
    id: string;
    anchor?: string;
    signature?: string;
    recipient?: string;
    owner?: {
        address: string;
        key: string;
    };
    fee?: {
        winston: string;
        ar: string;
    };
    quantity?: {
        winston: string;
        ar: string;
    };
    data?: {
        size: string;
        type: string;
    };
    tags?: Tag[];
    block?: {
        id: string;
        timestamp: number;
        height: number;
        previous: string;
    };
    parent?: {
        id: string;
    };
    bundledIn?: {
        id: string;
    };
}

// GQL Types
export interface GQLOptions {
    url?: string;
}

export interface GQLQueryOptions {
    first?: number;
    after?: string;
    asc?: boolean;
    next?: boolean;
    block?: {
        min?: number;
        max?: number;
    } | [number, number];
    id?: string;
    ids?: string[];
    recipient?: string;
    recipients?: string[];
    owner?: string;
    owners?: string[];
    tags?: Record<string, string>;
    fields?: string[] | Record<string, any>;
}

// HB (HyperBEAM) Types
export interface HBOptions {
    url: string;
}

export interface HBMessage {
    target: string;
    from?: string;
    to?: string;
}

export interface HBResult {
    Messages: any[];
    Spawns: any[];
    Assignments: any[];
    Output: any;
}

// Main Classes
export class AO {
    constructor(opt?: AOOptions);
    init(jwk: JWK): Promise<AO>;
    toSigner(wallet: JWK): Function;
    pipe(options: PipeOptions): Promise<any>;
    postModule(options: { data: string | Uint8Array, jwk: JWK, tags?: Record<string, string>, overwrite?: boolean }): Promise<string>;
    postScheduler(options: { jwk: JWK, url: string, tags?: Record<string, string>, overwrite?: boolean }): Promise<string>;
    spwn(options?: ProcessOptions): Promise<string>;
    res(options?: ResultOptions): Promise<any>;
    msg(options: Message): Promise<any>;
    asgn(options: { pid: string, mid: string, jwk: JWK, check?: any[], get?: boolean | string }): Promise<any>;
    dry(options: { pid: string, jwk: JWK, data?: string | Uint8Array, act?: string, tags?: Tag[], check?: any[], get?: boolean | string }): Promise<any>;
    ress(options: { pid: string, limit?: number, asc?: boolean, cursor?: string }): Promise<any>;
    eval(options: { pid: string, jwk: JWK, data: string | Uint8Array }): Promise<any>;
    transform(options: { src: string, data: any, fills: Record<string, any> }): Promise<string>;
    load(options: { src: string, data: any, fills: Record<string, any>, pid: string, jwk: JWK }): Promise<any>;
    wait(options: { pid: string, attempts?: number }): Promise<void>;
    attest(options: { id: string, jwk: JWK, tags: Tag[] }): Promise<any>;
    avail(options: { ids: string[], jwk: JWK, tags: Tag[] }): Promise<any>;
    deploy(options: DeployOptions): Promise<{ p: Process }>;
    var(options: { pid: string, data: any, json?: boolean, pretty?: boolean }): Promise<any>;
    p(pid: string): Process;
    ar: AR;
}

export class WeaveDB {
    constructor(ar: any, dir: string, options?: WeaveDBOptions);
    create(id: string): Promise<void>;
    createData(col: string, doc: any, val: any): Promise<void>;
    open(filename: string, val: any): Promise<void>;
    read(fd: number, raw_dst_ptr: number, raw_length: number, val: any): Promise<void>;
    close(fd: number): void;
}

export class WeaveDrive {
    constructor(ar: any, options?: WeaveDriveOptions);
    reset(fd: number): void;
    create(id: string): Promise<number>;
    read(fd: number, raw_dst_ptr: number, raw_length: number): Promise<number>;
    close(fd: number): void;
}

export class Process {
    constructor(pid: string, ao: AO);
    load(opt?: any): Promise<any>;
    msg(act: string, tags: Tag[], opts?: any): Promise<any>;
    dry(act: string, tags: Tag[], opts?: any): Promise<any>;
    res(opts?: any): Promise<any>;
    o(name: string, opt?: any): Promise<any>;
    m(...opt: any[]): Promise<any>;
    d(...opt: any[]): Promise<any>;
    v(data: any, json?: boolean, pretty?: boolean): Promise<any>;
    r(...opt: any[]): Promise<any>;
}

export class AR {
    constructor(options?: AROptions);
    init(jwk: JWK): Promise<AR>;
    gen(amount?: string): Promise<{ jwk: JWK, addr: string, pub: string, balance: string }>;
    toAddr(jwk: JWK): Promise<string>;
    mine(): Promise<void>;
    balance(addr?: string): Promise<string>;
    toAR(winston: string): string;
    toWinston(ar: string): string;
    transfer(amount: string, to: string, jwk?: JWK): Promise<{ id: string }>;
    checkWallet(options?: { jwk?: JWK }): Promise<{ err: any, jwk: JWK }>;
    post(options: { data: string | Uint8Array, tags?: Record<string, string>, jwk?: JWK }): Promise<{ err: any, id: string }>;
    tx(txid: string): Promise<ARTransaction>;
    data(txid: string, asString?: boolean): Promise<string | Uint8Array>;
    bundle(dataitems: Array<[string | Uint8Array, Record<string, string>]>): Promise<{ err: any, id: string }>;
    gql: GQL;
}

export class GQL {
    constructor(options?: GQLOptions);
    txs(options?: GQLQueryOptions): Promise<{ next?: () => Promise<any>, data: ARTransaction[] }>;
    blocks(options?: GQLQueryOptions): Promise<{ next?: () => Promise<any>, data: any[] }>;
}

export class HB {
    constructor(options: HBOptions);
    init(jwk: JWK): Promise<HB>;
    metrics(): Promise<any>;
    info(): Promise<any>;
    messages(options: HBMessage): Promise<any>;
    process(options?: { tags?: Tag[], data?: string }): Promise<string>;
    schedule(options: { process: string, tags?: Tag[], data?: string, action?: string }): Promise<number>;
    compute(options: { process: string, slot: number }): Promise<HBResult>;
    dryrun(options: { process: string, tags?: Tag[], data?: string, action?: string }): Promise<HBResult>;
    get(options: { device: string, path: string }): Promise<any>;
    post(options: { device: string, path: string, tags?: Tag[] }): Promise<any>;
    request(options: { device: string, path: string, tags?: Tag[], method: string }): Promise<any>;
}

// Utility Types
export interface Utils {
    allChecked: Function;
    searchTag: Function;
    checkTag: Function;
    wait: Function;
    isLocalhost: Function;
    tag: Function;
    ltags: Function;
    action: Function;
    isData: Function;
    getTagVal: Function;
    srcs: Function;
    buildTags: Function;
    isRegExp: Function;
    mergeChecks: Function;
    isCheckComplete: Function;
}

// Test Types
export interface TestAccount {
    jwk: JWK;
    addr: string;
    signer: Function;
}

export interface TestConnect {
    spawn: Function;
    message: Function;
    dryrun: Function;
    assign: Function;
    result: Function;
}

export function connect(mem?: any): TestConnect;
export const acc: TestAccount[]; 