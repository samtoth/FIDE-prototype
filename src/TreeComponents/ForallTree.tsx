import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {FIDETree, forall, termToTree, Context, Ident, Forall, Introduction, reduceAndNormalise} from "./FIDE-lang";

type ForallTag = { name: string, context: Context, erased: boolean, bindName: Ident }

export class ForallTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements Introduction<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {


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


    varType(): FIDETree<T1, C1> {
        return this.children[0];
    }

    substitute(idx: number, tree: Treeable<BasicTag, unknown>): FIDETree<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
        return {...this, children: [this.children[0].substitute(idx, tree), this.children[1].substitute(idx + 1, tree)]};
    }

    reduce<T1 extends BasicTag, C1>(): FIDETree<BasicTag, unknown> {
        throw this
    }

    normalise(): FIDETree<BasicTag, unknown> {
        // let eras = norm_weak.eras
        // let self = norm_weak.self
        // let name = norm_weak.name
        // let xtyp = Kind.Core.normalize(norm_weak.xtyp, defs)
        // let body = ((s, x) Kind.Core.normalize(norm_weak.body(s, x), defs)) :: Kind.Core.Term -> Kind.Core.Term -> Kind.Core.Term
        // Kind.Core.Term.all(eras, self, name, xtyp, body)
        const xtyp = reduceAndNormalise(this.children[0], context)
        const body = reduceAndNormalise(this.children[1], context)
        return {...this, children: [xtyp, body]}
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
                    <div>{this.children[1].display([...coord, 1], {...props, context: [...props.context, coord]})}</div>
                </div>
            </Foldable>
        </Selectable>)
    }

    desugar() {
        return forall(this.tag.erased, this.tag.bindName, this.children[0].desugar(), this.children[1].desugar());
    }

    contextProp(current: TreeIndex, target: TreeIndex, context: Context): Context {
        if (current === target) {
            return context;
        }else {
            switch (target[current.length]) {
                case 0: {
                    return this.children[0].contextProp([...current, 0], target, context)
                }
                case 1: {
                    return this.children[1].contextProp([...current, 1], target, [...context, current])
                }
                default: {
                    throw Error("Bad index given when gathering context")
                }
            }
        }
    }

}