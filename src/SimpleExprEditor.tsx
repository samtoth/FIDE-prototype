import CodeTree from './CodeTree'
import {cursor_child, cursor_parent, cursor_sibling_dec, cursor_sibling_inc, cursor_unset, editTree, TreeUUID, useFormDef} from './features/formDefinition'
import {useDispatch} from 'react-redux'
import {useState} from 'react'
import {info} from 'features/log'
import {foldToggle} from 'Treeable'
import {replaceWithHole} from 'Hole'
import HoleFillDialog from 'holeFillDialog'

interface ExprEditorProps {
    treeId: TreeUUID | undefined,
    className: string | undefined
}

const SimpleExprEditor: (p: ExprEditorProps) => JSX.Element = ({treeId, className}) => {
    const trees = useFormDef()
    const tree = treeId === undefined ? trees.mainTree : trees.otherTrees.get(treeId)
    const cname = (className || "") + " flex flex-row"
    const dispatch = useDispatch()

    const [active, setActive] = useState(false)

    const keyHandler = (e) => {
        //console.log(e)
        if (e.code != "Tab") {
            e.preventDefault()
        }
        dispatch(info(`keydown ${e.code}`, {timeout: 3000}))
        dispatch(KeyMapping(treeId)[e.code])
    }

    if (tree === undefined) {
        return (<div className={className}>Error tree {treeId} not found</div>)
    } else {
        return (
            <div className={cname}
                tabIndex={0}
                onKeyDown={keyHandler}
                onFocus={() => setActive(true)}
                onBlur={(_) => setActive(false)}>

                <CodeTree
                    tree={tree.tree}
                    selected={tree.cursor}
                    className="p-5 mr-5 font-semibold text-stone-100 bg-stone-700 shadow-inner max-h-full overflow-y-scroll"
                    coord={[]}
                    treeID={undefined} />
                <div className="grow min-h-full cursor-default m-10" id={`tree${treeId}`} onClick={(_) => dispatch(cursor_unset({id: treeId}))}>
                    {tree.tree.display([], {active: active, highlights: [], treeId: treeId})}
                </div>

            </div >
        )

    }
}

const KeyMapping = (treeId: TreeUUID) => {
    return {
        "ArrowLeft": cursor_sibling_dec({id: treeId}),
        "ArrowRight": cursor_sibling_inc({id: treeId}),
        "ArrowUp": cursor_parent({id: treeId}),
        "ArrowDown": cursor_child({id: treeId}),
        "KeyF": editTree({func: foldToggle, id: treeId}),
        "Delete": editTree({func: replaceWithHole, id: treeId}),
        "Space": info("fill hole", {timeout: 3000})
    }
}

export default SimpleExprEditor
