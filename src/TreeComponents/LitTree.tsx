import {DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Selectable} from "../CodeViewFeatures";
import {Context, FIDETree, lit, Char, FormLit} from "./FIDE-lang";

type LitTag = {
    name: string,
    val: FormLit,
    context: Context
}

export class LitTree
    extends Treeable<LitTag, []>
    implements FIDETree<LitTag, []> {
    constructor(lit: FormLit, context: Context, i: TreeIndex) {
        super({name: "lit " + lit, val: lit, context: context}, false, [])
    }

    desugar() {
        return lit(this.tag.val)
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active}><span>{this.tag.val}</span></Selectable>)
    }
}