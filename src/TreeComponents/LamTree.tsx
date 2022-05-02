import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {Context, FIDETree, Ident, lam, Lam, termToTree, Introduction, reduceAndNormalise} from "./FIDE-lang";
import {produce} from "immer"

type LamTag = {
    name: string,
    bindName: Ident
}

export class LamTree<T extends BasicTag, C>
    extends Treeable<LamTag, [Treeable<T, C>]>
    implements Introduction<LamTag, [Treeable<T, C>]> {

    constructor(bindName: string, body: Treeable<T, C>){
        super(
            {
                name: "lam " + bindName,
                bindName: bindName
               },
            false,
            [body]
        )
    }


    static fromForm(term: Lam, context: Context, index: TreeIndex) {
        const [bindName, rhs] = term
        return new LamTree(bindName, termToTree(rhs, [...context, index], [...index, 0]))
    }



    normalise(): FIDETree<BasicTag, unknown> {
        // let name = norm_weak.name
        // let body = (x) Kind.Core.normalize(norm_weak.body(x), defs)
        // Kind.Core.Term.lam(name, body)

        const body = reduceAndNormalise(this.children[0])

        return produce(this, draft => {
            draft.children = [body]
        })
    }

    reduce(): FIDETree<BasicTag, unknown> {
        return this
    }

    varType<T1 extends BasicTag, C1>(): FIDETree<T1, unknown> {
        throw new Error("unimplemented method")
    }

    desugar() {
        return lam(this.tag.bindName, this.children[0].desugar())
    }

    varName(): string {
        return this.tag.bindName
    }

    substitute(idx: number, tree: Treeable<BasicTag, unknown>): FIDETree<LamTag, [FIDETree<T, C>]> {
        return {...this, children: [this.children[0].substitute(idx + 1, tree)]}
    }

    display(coord: TreeIndex, props: DisplayProps) {
        //const rhs = tree.folded ? "(...)" : latexTree(tree.children[0])
        //return ("\\lambda " + tree.tag.bindName + ".\\ " + rhs)
        return (
            <Selectable coord={coord} active={props.active}>
                <div className="flex space-x-2">
                    <div className="katex base">
                        <span className="mord mathnormal">λ</span>
                        <span className="mord text textit">{this.tag.bindName}.</span>
                    </div>
                    <Foldable folded={this.folded}>
                        {this.children[0].display([...coord, 0], {...props, context: [...props.context, coord]})}
                    </Foldable>
                </div>
            </Selectable>)
    }

    contextProp(current: TreeIndex, target: TreeIndex, context: Context): Context {
        if (current === target) {
            return context;
        }else {
            switch (target[current.length]) {
                case 0: {
                    return this.children[0].contextProp([...current, 0], target, [...context, current])
                }
                default: {
                    throw new Error("Bad index given when gathering context")
                }
            }
        }
    }
}
