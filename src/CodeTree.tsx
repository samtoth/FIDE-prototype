import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faCaretRight, faAngleDown} from '@fortawesome/free-solid-svg-icons'
import {editTreeAt, cursor_set, useFormDef, TreeUUID} from './features/formDefinition'
import {useDispatch} from 'react-redux'
import {BasicTag, foldToggle, index_to_string, Treeable, TreeIndex} from './Treeable'


interface CodeTreeProps<T extends BasicTag, C> {
    tree: Treeable<T, C>,
    selected: TreeIndex[],
    className: string | undefined,
    coord: TreeIndex,
    treeID: TreeUUID | undefined,
}

const CodeTree = (props: CodeTreeProps<T, C>) => {
    let tree = props.tree
    let coord = props.coord

    const dispatch = useDispatch()

    const selected = props.selected.map((c) => index_to_string(c) === index_to_string(coord)).reduce((prev, cur, _i, _arr) => prev || cur, false)


    if (tree != null) {
        return (
            <li className={props.className || "" + "ml-4 "} onClick={(e) => {e.stopPropagation(); dispatch(cursor_set({at: coord}))}} key={tree.tag.name + ":" + index_to_string(coord)}>
                <div className={`${selected ? "underline text-blue-400" : "hover:text-blue-100"} cursor-default`}>
                    {tree.children.length >= 1 ? <span className="-ml-3 cursor-pointer" onClick={e => {e.stopPropagation(); dispatch(editTreeAt({at: [coord], func: foldToggle}))}}>{
                        tree.folded ? <FontAwesomeIcon icon={faCaretRight} /> : <FontAwesomeIcon icon={faAngleDown} />
                    }</span> : <></>}
                    <span className="pl-3 mr-2">{tree.tag.name}</span>
                </div>
                {
                    (!tree.folded && (tree.children.length > 0)) &&
                    <ul className={`list-inside border-l-2 pb-1 bg-none ${selected ? "border-blue-400" : "border-gray-200"}`}>
                        {tree.children.map((child, i) => {
                            return (
                                <CodeTree tree={child} coord={[...coord, i]} selected={props.selected} />
                            )
                        })}
                    </ul>

                }
            </li >
        )
    } else {
        return <code> error... missing tree item </code>
    }
}

export default CodeTree;
