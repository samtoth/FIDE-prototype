import {useEffect, useState} from "react"
import {useDispatch} from "react-redux"
import {info, useLogs} from "./features/log"

const infoMessage = (message) => {
    return (<div>{message}</div>)
}


const Messages = (props) => {

    const logs = useLogs()
    const dispatch = useDispatch()

    const [time, setTime] = useState(Date.now())
    useEffect(() => {
        const interval = setInterval(() => setTime(Date.now()), 1000);
        return () => {
            clearInterval(interval);
        };
    }, []);

    useEffect(() => {
        dispatch(info("myMessage", {timeout: 5000}))
    }, [])

    return (
        <div className="bottom-0 right-0 absolute">
            <ul>
                {logs.messages.filter((value) => {
                    if (value.timeout) {
                        return ((time - value.timestamp) < value.timeout)
                    } else {
                        return true
                    }
                }).map((value) => {
                    return (
                        <li className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded m-2">
                            {value.message}
                        </li>
                    )
                })}
            </ul>
        </div>
    )
}

export default Messages
