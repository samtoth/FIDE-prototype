import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Selectable} from "../CodeViewFeatures";
import {Context, FIDETree, ty} from "./FIDE-lang";

type TypeTag = {
    name: string,
    context: Context
}

export class TypeTree
    extends Treeable<TypeTag, []>
    implements FIDETree<TypeTag, []> {
    constructor(context: Context, _i: TreeIndex) {
        super({name: "type", context: context}, false, [])
    }

    desugar() {
        return ty
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return <Selectable coord={coord} active={props.active}><span className="katex katex-html base"><span
            className="mord">⋆</span></span></Selectable>
    }

    normalise(): FIDETree<BasicTag, unknown> {
        return this;
    }

    reduce(): FIDETree<BasicTag, unknown> {
        return this;
    }

    substitute<T1 extends BasicTag, C1>(idx: number, tree: Treeable<T1, C1>): FIDETree<TypeTag, []> | Treeable<T1, C1> {
        return this
    }
}