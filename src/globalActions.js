import {useEffect, useState} from 'react'

export const useGlobalActions = () => {

    const [keyStack, setKeyStack] = useState([])
    const [actionStack, setActionStack] = useState([])

    const pushKey = (newKey) => {
        setKeyStack(key => key + newKey)
    }

    const pushAction = (action) => {
        setKeyStack(a => a + action)
    }

    const downHandler = e => {
        if (!key.repeat) {
            const modi = [e.ctrlKey, e.altKey, e.shiftKey, metaKey]
            return (pushKey({key: e.key, modi: modi}))
        }
    }

    useEffect(() => {
        window.addEventListener("keydown", downHandler);
        // Remove event listeners on cleanup
        return () => {
            window.removeEventListener("keydown", downHandler);
        };
    }, []);

    const subscribeToAction = () => {

    }

    const sendAction = () => {

    }

    return [subscribeToAction, sendAction]
}
