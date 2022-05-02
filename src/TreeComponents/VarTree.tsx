import {BasicTag, DisplayProps, getAt, Treeable, TreeIndex} from "./Treeable";
import {Selectable} from "../CodeViewFeatures";
import {Context, contextGet, FIDETree, fvar, Var} from "./FIDE-lang";
import {getTree, TreeUUID, useFormDef} from "../features/formDefinition";
import React from "react";

type VarTag = {
    name: string,
    varIdx: number
}

export class VarTree
    extends Treeable<VarTag, []>
    implements FIDETree<VarTag, []> {

    constructor(term: Var, context?: Context, i?: TreeIndex) {
        const [index] = term
        super({name: `var: ${index}`, varIdx: index}, false, [])
    }

    normalise<T1 extends BasicTag, C1>(): FIDETree<BasicTag, unknown> {
        return this;
    }

    reduce<T1 extends BasicTag, C1>(): FIDETree<BasicTag, unknown> {
        return this;
    }

    display(coord: TreeIndex, props: DisplayProps) {

        const f = () => {
            let varCoord = contextGet(props.context, this.tag.varIdx)
            if (varCoord === undefined) {
                return <>var <span className="text-sm text-red-600">error</span></>
            } else {
                return (<><NameOf id={undefined} coord={varCoord}/><span
                    className="text-sm">{this.tag.varIdx}</span></>)
            }
        }
            return (<Selectable coord={coord} active={props.active} treeId={undefined}>
                <span className="katex katex-html"><span className="mord text textit">
                {
                    f()
                }
                </span></span>
            </Selectable>)

    }

    desugar() {
        return fvar(this.tag.varIdx)
    }

    substitute<T extends BasicTag, C>(idx: number, tree: Treeable<T, C>) {
        if (this.tag.varIdx === idx) {
            return tree
        } else {
            return this
        }
    }
}


const NameOf = (props: {coord: TreeIndex, id: TreeUUID}) => {
    const trees = useFormDef()
    const tree = getTree(trees, props.id)


    return (<>{getAt(props.coord, tree?.tree).varName()}</>)
}