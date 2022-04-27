import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {Context, FIDETree, FormDefinition, Ident, makeDef, termToTree} from "./FIDE-lang";

type DefTag = {
    name: string,
    defName: Ident,
}

export class DefTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<DefTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<DefTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
    constructor(term: FormDefinition, context: Context, i: TreeIndex) {
        const [name, ty, expr] = term
        super({
            name: "def: " + name,
            defName: name
        }, false, [termToTree(ty, context, [...i, 0]), termToTree(expr, context, [...i, 1])])
    }

    desugar() {
        return makeDef(this.tag.bindName, this.children[0].desugar(), this.children[1].desugar())
    }

    display(coord: TreeIndex, props: DisplayProps) {
        //return (`${tree.tag.defName}^{${latexTree(tree.children[0])}}\\ \\leftarrow\\ ${latexTree(tree.children[1])}`)
        return (
            <Selectable coord={coord} treeId={undefined} active={props.active}>
                {this.folded ? <div className="font-bold">{this.tag.defName}</div> : <></>}
                <Foldable folded={this.folded}>
                    <div className="flex flex-col text-lg">
                        <div className="flex space-x-2"><span className="font-bold">{this.tag.defName}</span>
                            <span>:</span> {this.children[0].display([...coord, 0], props)}</div>
                        <div className="flex space-x-2"><span className="font-bold">{this.tag.defName}</span>
                            <span>=</span> {this.children[1].display([...coord, 1], props)}</div>
                    </div>
                </Foldable>
            </Selectable>
        )
    }
}