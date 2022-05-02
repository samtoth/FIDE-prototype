import {BasicTag, DisplayProps, idx_eql, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {app, App, Context, FIDETree, reduceAndNormalise, termToTree} from "./FIDE-lang";
import {LamTree} from "./LamTree";
import { current, produce } from "immer";

type AppTag = {
    name: string,
}

// let func = Kind.Core.reduce(term.func, defs)
// case func {
//     lam: Kind.Core.reduce(func.body(term.argm), defs)
// } default Kind.Core.Term.app(func, term.argm)

export class AppTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<AppTag, [Treeable<T1, C1>, Treeable<T2, C2>]>
    implements FIDETree<AppTag, [Treeable<T1, C1>, Treeable<T2, C2>]> {

    constructor(func: Treeable<T1, C1>, argm: Treeable<T2, C2>){
        super({
            name: "app",
        },
        false, [func, argm])
    }

    contextProp(coord: TreeIndex, target: TreeIndex, context: Context): Context {
        if (idx_eql(coord, target)) {
            return context;
        }else {
            console.log("target", current(target), "current", coord)
            switch (target[coord.length]) {
                case 0: {
                    return this.children[0].contextProp([...coord, 0], target, context)
                }
                case 1: {
                    return this.children[1].contextProp([...coord, 1], target, context)
                }
                default: {
                    throw new Error("Bad index given when gathering context")
                }
            }
        }
    }

    static fromForm(term: App, context: Context, index: TreeIndex): AppTree<BasicTag, unknown, BasicTag, unknown> {
        const [func, arg] = term
        return new AppTree(termToTree(func, context, [...index, 0]), termToTree(arg, context, [...index, 0]))
    }

    substitute(idx: number, tree: Treeable<BasicTag, unknown>): FIDETree<AppTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
        return {...this, children: this.children.map((t => t.substitute(idx, tree)))};
    }

    reduce(): FIDETree<BasicTag, unknown> {
        const func = this.children[0].reduce()
        if (func.constructor.name === "LamTree") {
            const lam: LamTree<BasicTag, unknown> = func
            return (lam.children[0].substitute(0, this.children[1].reduce()))
        } else {
            return produce(this, draft => {
                draft.children = [func, this.children[1]]
            })
        }
    }
    normalise(): FIDETree<BasicTag, unknown> {
        // let func = Kind.Core.normalize(norm_weak.func, defs)
        // let argm = Kind.Core.normalize(norm_weak.argm, defs)
        // Kind.Core.Term.app(func, argm)
        const func = reduceAndNormalise(this.children[0])
        const argm = reduceAndNormalise(this.children[1])
        throw {...this, children: [func, argm]}
    }

    desugar() {
        return app(this.children[0].desugar(), this.children[1].desugar())
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active}>
                <div className="flex space-x-2">
                    <span>(</span>
                    <Foldable folded={this.folded}>
                        <>
                            {this.children[0].display([...coord, 0], props)}
                            {this.children[1].display([...coord, 1], props)}
                        </>
                    </Foldable>
                    <span>)</span>
                </div>
            </Selectable>
        )
    }
}