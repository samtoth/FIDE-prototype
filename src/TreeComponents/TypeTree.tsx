import {DisplayProps, Treeable, TreeIndex} from "./Treeable";
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

    display(coord, props: DisplayProps) {
        return <Selectable coord={coord} active={props.active}><span className="katex katex-html base"><span
            className="mord">⋆</span></span></Selectable>
    }
}