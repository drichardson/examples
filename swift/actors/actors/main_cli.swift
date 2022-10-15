import Foundation

@main
struct CLIRunner {
    static func main() async {
        print("in main")

        let a1 = Actor1(string: "a1")

        print("awaiting 1")
        print(await a1.mutable)
        print(a1.onlyConstant())
        print("immutableString: \(a1.immutableString)")
        print("getMutable: \(await a1.getMutable()) mutable=\(await a1.mutable)")
        await a1.mutate()
        print("getMutable: \(await a1.getMutable()) mutable=\(await a1.mutable)")
        let t = Task {
            async let t1 = a1.longOperation()
            async let t2 = a1.longOperation()
            async let t3 = a1.longOperation()
            print("await [t1,t2,t3] in task: \(await [t1, t2, t3])")
            print("extra await: \(await a1.longOperation())")
            print("DONE task")
        }
        async let t1 = a1.longOperation()
        async let t2 = a1.longOperation()
        async let t3 = a1.longOperation()
        print("await [t1,t2,t3] outside task: \(await [t1, t2, t3])")
        print("await t.value: \(await t.value)")
        print("async without await")
        async let t4 = a1.longOperation()
        async let t5 = a1.longOperation()
        print("sleeping main")
        sleep(1)
        // _ = await [t4, t5]
        print("done")
    }
}
