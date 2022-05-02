import {BasicTag, Displayable, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {FIDETree, FormCore} from "./FIDE-lang";
import {Foldable, Selectable} from "../CodeViewFeatures";
import React from "react";


interface LetTag {
    name: string,
}

export default class LetTree
    extends Treeable<LetTag, Array<Displayable<BasicTag, any>>>
    implements FIDETree<LetTag, Array<Displayable<BasicTag, any>>>
{
    constructor(initBinds: Array<Displayable<BasicTag, any>>, body: Displayable<BasicTag, any>){
        super(
            {name: "let"},
            false,
            [...initBinds,  body]
        )
    }

    desugar(): FormCore {
        return undefined;
    }

    display(coord: TreeIndex, props: DisplayProps): JSX.Element {
        return <Selectable coord={coord} active={props.active} treeId={props.treeId} >
            <Foldable  folded={this.folded}>
                <div className="flex flex-col katex katex-html">
                    <div className="ml-1"><span className="textit">let</span></div>
                    <div className="ml-3">
                        {this.children.slice(0, this.children.length - 1).map((c, idx) => c.display([...coord, idx], props))}
                    </div>
                    <div className="ml-1">
                        <span className="textit">in</span>
                    </div>
                    <div className="ml-3">
                        {this.children[this.children.length-1].display([...coord, this.children.length-1], props)}
                    </div>
                </div>
            </Foldable>
        </Selectable>
    }

    normalise(): FIDETree<BasicTag, unknown> {
        return undefined;
    }

    reduce(): FIDETree<BasicTag, unknown> {
        return undefined;
    }

    substitute<T1 extends BasicTag, C1>(idx: number, tree: Treeable<T1, C1>): FIDETree<LetTag, Treeable<BasicTag, any>[]> | Treeable<T1, C1> {
        return undefined;
    }
}