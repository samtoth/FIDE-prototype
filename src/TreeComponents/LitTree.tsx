import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
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

    normalise(): FIDETree<BasicTag, unknown> {
        return undefined;
    }

    reduce(): FIDETree<BasicTag, unknown> {
        return undefined;
    }

    substitute<T1 extends BasicTag, C1>(idx: number, tree: Treeable<T1, C1>): FIDETree<LitTag, []> | Treeable<T1, C1> {
        return this
    }
}