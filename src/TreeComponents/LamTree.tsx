import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {Context, FIDETree, Ident, lam, Lam, termToTree, Introduction} from "./FIDE-lang";

type LamTag = {
    name: string,
    bindName: Ident,
    context: Context,
}

export class LamTree<T extends BasicTag, C>
    extends Treeable<LamTag, [FIDETree<T, C>]>
    implements Introduction<LamTag, [FIDETree<T, C>]> {
    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        return undefined;
    }

    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        return undefined;
    }

    varType<T1 extends BasicTag, C1>(): FIDETree<T1, C1> {
        return undefined;
    }

    constructor(term: Lam, context: Context, index: TreeIndex) {
        const [bindName, rhs] = term
        super({
            name: "lam " + bindName,
            bindName: bindName,
            context: context
        }, false, [termToTree(rhs, [...context, index], [...index, 0])])
    }

    desugar() {
        return lam(this.tag.bindName, this.children[0].desugar())
    }

    varName(): string {
        return this.tag.bindName
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
                        {this.children[0].display([...coord, 0], props)}
                    </Foldable>
                </div>
            </Selectable>)
    }
}