import {createSlice} from '@reduxjs/toolkit'
import {useDispatch, useSelector} from 'react-redux'
import {getAt, editAt, Treeable, TreeIndex, BasicTag} from '../TreeComponents/Treeable'
import {
    forall,
    lam,
    app,
    lit,
    fvar,
    termToTree,
    makeDef,
    ty,
    printFormJs,
    reduceAndNormalise, gatherContext, FIDETree
} from '../TreeComponents/FIDE-lang'
import {fmc_to_js} from 'formcore-js'
import {error} from './log'
import {Map} from 'immutable';
import produce, {current} from 'immer'
import {cons} from 'fp-ts/lib/ReadonlyArray'

const constrainCursor = <T extends BasicTag, C>(cursor: TreeIndex, tree: Treeable<T, C>): TreeIndex => {
    if (cursor.length === 0) {
        return cursor
    } else {
        if (tree.children.length === 0) {
            return []
        }


        const [head, ...tail] = cursor
        if (head < 0) {
            return [0]
        }
        if (head > tree.children.length - 1) {
            return [tree.children.length - 1]
        }

        return ([head, ...constrainCursor(tail, tree.children[head])])
    }
}

export type Tree = {cursor: TreeIndex[], tree: Treeable<BasicTag, unknown>}
export type TreeUUID = number | undefined

export type LoadedTreeState = {
    mainTree: Tree,
    otherTrees: Map<TreeUUID, Tree>,
    mainCompiledCode: String | undefined
}

const tree = makeDef("main",
    forall(true, "A", ty, forall(false, "f", forall(false, "n", fvar(0), fvar(1)), forall(false, "a", fvar(1), fvar(2)))),
    lam("A",
        lam("f",
            lam("x",
                app(fvar(1), app(fvar(1), fvar(0))
                ))
        )
    ))

console.log(printFormJs(tree))

const initialState: LoadedTreeState = {
    mainTree: {
        cursor: [],
        tree: termToTree(tree, [], []),
    },
    otherTrees: Map(),
    mainCompiledCode: undefined,
}

export const getTree = (state: LoadedTreeState, id: TreeUUID | undefined) => {
    return (id ? state.otherTrees.get(id) : state.mainTree)
}

const setTree: (state: LoadedTreeState, id: TreeUUID | undefined, tree: Tree) => LoadedTreeState = (state, id, tree) => {
    if (id) {
        return {
            ...state,
            otherTrees: state.otherTrees.set(id, tree)
        }
    } else {
        return {
            ...state,
            mainTree: tree
        }
    }
}

export const loadedTreeSlice = createSlice({
    name: 'formDef',
    initialState,
    reducers: {
        cursor_sibling_inc: (state, action) => {
            // @ts-ignore
            const tree = getTree(state, action.payload.id)
            if (tree === undefined) {
                throw Error("undefined tree")
            }
            const cursor = produce(tree?.cursor[tree?.cursor.length - 1], draft => {
                if (draft.length === 0) {
                    draft.push(0)
                } else {
                    const parent = draft.slice(0, -1)
                    const curr = current(current(draft))
                    //console.log("draft: ", curr, "parent: ", parent)
                    const treeParent = getAt(parent, tree?.tree)
                    //console.log("new length: ", draft[draft.length - 1] + 1, "number sibs", treeParent.children.length)
                    if (draft[draft.length - 1] + 1 >= treeParent.children.length) {
                        if (treeParent.children[draft[draft.length - 1]].children.length != 0) {
                            draft.push(0)
                        }
                    } else {
                        draft[draft.length - 1] += 1
                    }

                }
            })
            return setTree(state, action.payload.id, {...tree, cursor: [cursor]})
        },
        cursor_sibling_dec: (state, action) => {
            const tree = getTree(state, action.payload.id)
            if (tree === undefined) {
                throw Error("undefined tree")
            }
            let cursor = produce(tree?.cursor[tree?.cursor.length - 1], draft => {
                const parent = draft.slice(0, -1)
                const treeParent = getAt(parent, tree.tree)
                if (draft[draft.length - 1] - 1 < 0) {
                    //console.log(current(current(draft)))
                    return draft.slice(0, draft.length - 1)
                } else {
                    draft[draft.length - 1] -= 1
                }
            })
            return setTree(state, action.payload.id, {...tree, cursor: [constrainCursor(cursor, tree.tree)]})
        },
        cursor_parent: (state, action) => {
            const tree = getTree(state, action.payload.id)
            if (tree === undefined) {
                throw Error("undefined tree")
            }
            if (tree?.cursor[0].length === 0) {
                return state
            } else {
                return setTree(state, action.payload.id, {...tree, cursor: [tree.cursor[0].slice(0, -1)]})
            }
        },

        cursor_child: (state, action) => {
            const tree = getTree(state, action.payload.id)
            return setTree(state, action.payload.id, {...tree, cursor: [constrainCursor([...tree.cursor[0], 0], tree.tree)]})
        },
        cursor_unset: (state, action) => {
            const tree = getTree(state, action.payload.id)
            return setTree(state, action.payload.id, {...tree, cursor: []})
        },
        cursor_set: (state, action) => {
            //action.payload
            const tree = getTree(state, action.payload.id)
            return setTree(state, action.payload.id, {...tree, cursor: [constrainCursor(action.payload.at, tree.tree)]})
        },
        editTree: (state, action) => {
            const tree = getTree(state, action.payload.id)
            return setTree(state, action.payload.id, {...tree, tree: editAt(tree.cursor, action.payload.func, tree.tree)})
        },
        editTreeAt: (state, action) => {
            const tree = getTree(state, action.payload.id)
            return setTree(state, action.payload.id, {...tree, tree: editAt([action.payload.at], action.payload.func, tree.tree)})
        },
        replaceWithNormal: (state, action) => {
            const tree = getTree(state, action.payload.id)
            if (tree == null){
                return state
            }

            const at: TreeIndex[] = [(() => {if (action.payload.at == null) {
                return tree?.cursor[0]
            } else {
                return action.payload.at
            }})()]

            //const context = gatherContext(tree.tree as FIDETree<BasicTag, unknown>, at[0])

            const func = (t) => {
                if (t.normalise) {
                    return reduceAndNormalise(t)
                }else {
                    return t
                }
            }

            return setTree(state, action.payload.id, {...tree, tree: editAt(at, func, tree?.tree)})
        },
        compile: (state) => {
            try {
                console.log(state.tree.tag.printProgram(state.tree))
                const output = fmc_to_js.compile(state.tree.tag.printProgram(state.tree), state.tree.tag.defName)
                return {...state, compiledCode: output}
            }
            catch (err) {
                console.log(err)
                return {...state}
            }
        }
    }
})

export const useFormDef: () => LoadedTreeState = () => {
    return useSelector(state => state.formDef)
}

// Action creators are generated for each case reducer function
export const {cursor_sibling_inc, cursor_sibling_dec, cursor_parent, cursor_child, cursor_unset, cursor_set, editTree, editTreeAt, replaceWithNormal, compile} = loadedTreeSlice.actions

export default loadedTreeSlice.reducer
