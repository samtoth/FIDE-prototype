import {BasicTag, DisplayProps, getAt, Treeable, TreeIndex} from "./Treeable";
import {Selectable} from "../CodeViewFeatures";
import {Context, contextGet, FIDETree, fvar, Var} from "./FIDE-lang";
import {getTree, TreeUUID, useFormDef} from "../features/formDefinition";

type VarTag = {
    name: string,
    varIdx: number,
    context: Context
}

export class VarTree
    extends Treeable<VarTag, []>
    implements FIDETree<VarTag, []> {
    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        return undefined;
    }

    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        return undefined;
    }
    constructor(term: Var, context: Context, i: TreeIndex) {
        const [index] = term
        super({name: `var: ${index}`, varIdx: index, context: context}, false, [])
    }

    display(coord: TreeIndex, props: DisplayProps) {
        let varCoord = contextGet(this.tag.context, this.tag.varIdx)
        if (varCoord === undefined) {
            varCoord = this.tag.context[0]
        }
        return (<Selectable coord={coord} active={props.active} treeId={undefined}>
            <span className="katex katex-html"><span className="mord text textit">
                <NameOf id={undefined}  coord={varCoord}/><span
                className="text-sm">{this.tag.varIdx}</span></span></span>
        </Selectable>)
    }

    desugar() {
        return fvar(this.tag.varIdx)
    }
}


const NameOf = (props: {coord: TreeIndex, id: TreeUUID}) => {
    const trees = useFormDef()
    const tree = getTree(trees, props.id)


    return (<>{getAt(props.coord, tree?.tree).varName()}</>)
}