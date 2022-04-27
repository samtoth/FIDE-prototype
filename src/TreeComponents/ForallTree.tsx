import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {FIDETree, forall, termToTree, Context, Ident, Forall} from "./FIDE-lang";

type ForallTag = { name: string, context: Context, erased: boolean, bindName: Ident }

export class ForallTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {

    constructor(term: Forall, context: Context, index: TreeIndex) {
        const [erased, ident, ty, rty] = term

        super(
            {
                name: "forall",
                erased: erased,
                bindName: ident,
                context: context
            },
            false,
            [termToTree(ty, context, [...index, 0]), termToTree(rty, [...context, index], [...index, 1])]
        )
    }

    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        throw new Error('Method not implemented.')
    }

    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        throw new Error('Method not implemented.')
    }

    varName() {
        return this.tag.bindName
    }


    display(coord: TreeIndex, props: DisplayProps) {
        //const innerparam = (s, e) => (s + this.bindName + " ^{" + latexTree(tree.children[0]) + "}" + e)
        //const param = tree.tag.erased ? innerparam("{{ ", " }}") : innerparam("( ", " )")
        //const rhs = tree.folded ? "(...)" : latexTree(tree.children[1])
        //return ("\\forall " + param + ".\\ " + rhs)

        return (<Selectable coord={coord} treeId={undefined} active={props.active}>
            <Foldable folded={this.folded}>
                <div className="flex space-x-2">
                    <div className="flex space-x-2">
                        <div className="katex katex-html base">
                            <span className="mord">{this.tag.erased ? '∀' : 'Π'}</span>
                            <span className="mopen">(</span>
                            <span className="mord text textit">{this.tag.bindName}</span>
                            <span className="mrel mx-1">:</span>

                        </div>
                        {this.children[0].display([...coord, 0], props)}
                        <div className="katex katex-html base">
                            <span className="mclose">)</span>
                            <span className="mord">.</span>
                        </div>
                    </div>
                    <div>{this.children[1].display([...coord, 1], props)}</div>
                </div>
            </Foldable>
        </Selectable>)
    }

    desugar() {
        return forall(this.tag.erased, this.tag.bindName, this.children[0].desugar(), this.children[1].desugar());
    }

}