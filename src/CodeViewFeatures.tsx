import {cursor_set, getTree, LoadedTreeState, TreeUUID, useFormDef} from "features/formDefinition"
import {useDispatch} from "react-redux"
import {index_to_string, TreeIndex} from "TreeComponents/Treeable"

type SelectableProps = {
    treeId: TreeUUID | undefined,
    coord: TreeIndex,
    children: JSX.Element,
    active: boolean
}

export const Selectable = (props: SelectableProps) => {
    const dispatch = useDispatch()

    const trees: LoadedTreeState = useFormDef()
    const tree = getTree(trees, props.treeId)

    const selected = tree?.cursor.map((c) => index_to_string(c) === index_to_string(props.coord)).reduce((prev, cur, _i, _arr) => prev || cur, false)

    const borderColor = props.active ? "blue-300" : "slate-300"

    return (<div className={(selected ? `border-${borderColor} border-2 border-dashed p-1 -m-1` : "") + " "}
        onClick={(e) => {e.stopPropagation(); dispatch(cursor_set({at: props.coord, id: props.treeId}))}}>
        {props.children}
    </div>)
}

export const Foldable = (props: {folded: boolean, children: JSX.Element}) => {
    return (props.folded ? <div className="bg-slate-200 p-1">...</div> : props.children)
}


