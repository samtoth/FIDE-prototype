import {useFormDef, compile, LoadedTreeState} from './features/formDefinition'
import {useDispatch} from 'react-redux'
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faBrain, faPlay} from '@fortawesome/free-solid-svg-icons'
import Messages from './messages'
import SimpleExprEditor from 'SimpleExprEditor'

function App(): JSX.Element {

    const program: LoadedTreeState = useFormDef()
    const dispatch = useDispatch()

    return (
        <div className="flex flex-col min-h-screen">
            <div className="bg-stone-800 text-stone-100 shadow-md flex space-x-5">
                <h1 className="text-xl  p-5 mr-5"> Ellena's <span className="font-bold">F</span>unctional  <span className="font-extrabold">IDE</span></h1>
                <button onClick={_ => dispatch(compile())}><FontAwesomeIcon icon={faBrain} /></button>
                <button onClick={_ => {dispatch(compile());}}><FontAwesomeIcon icon={faPlay} /></button>
            </div>
            <SimpleExprEditor className="grow" treeId={undefined} />
            <Messages />
        </div >
    )
}

export default App


/*
 *
            <ReflexContainer orientation="horizontal" className="flex-grow" >
                <ReflexElement >
                    <ReflexContainer orientation="vertical">
                        <ReflexElement>
                            <CodeTree
                                tree={program.tree}
                                selected={program.cursor}
                                className="p-5 mr-5 font-semibold "
                                coord={[]} />
                        </ReflexElement>
                        <ReflexSplitter />
                        <ReflexElement className="p-5">
                            <LatexView />
                        </ReflexElement>
                    </ReflexContainer>
                </ReflexElement>
                <ReflexSplitter />
                <ReflexElement className="">
                    <Tabs selectedTabClassName="bg-stone-100 text-stone-900 font-bold hover:text-stone-900">
                        <TabList className="flex flex-row space-x-5 bg-stone-600 text-stone-100" >
                            <Tab className="p-2 hover:text-blue-400 cursor-default" ><FontAwesomeIcon icon={faCog} className="mr-2" />properties</Tab>
                            <Tab className="p-2 hover:text-blue-400 cursor-default"><FontAwesomeIcon icon={faCubesStacked} className="mr-2" />desugaring</Tab>
                            {program.compiledCode ? <Tab className="p-2 hover:text-blue-400 cursor-default"><FontAwesomeIcon icon={faJs} className="mr-2" />output</Tab> : <></>}
                        </TabList>

                        <TabPanel>
                            <h2>Any content 1</h2>
                        </TabPanel>
                        <TabPanel>
                            <code className="p-2">
                                {printFormJs(desugarTree(getAt(program.cursor, program.tree)))}
                            </code>
                        </TabPanel>
                        {program.compiledCode ?
                            <TabPanel>
                                <code className="m-2">
                                    {program.compiledCode}
                                </code>
                            </TabPanel>
                            :
                            <></>
                        }
                    </Tabs>
                </ReflexElement>

            </ReflexContainer >
 *
 *
 *
 * 
 */
