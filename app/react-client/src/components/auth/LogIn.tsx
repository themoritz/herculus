import React from "react"
import { Link, useNavigate } from "react-router-dom"
import { Row, Submit } from "../utils/Templates"
import { Kind, useStore, Config } from "../Notifications"
import { useStore as useGlobalStore } from "../../store"

type Tag = 'LoginSuccess' | 'LoginFailed'

type Email = {
    unEmail: string
}

type LoginSuccess = {
    _uiSessionKey: string
    _uiUserEmail: Email
    _uiUserId: string
    _uiUserName: string
}

type LoginResponse = {
    tag: Tag,
    contents: string | LoginSuccess
}

export const LogIn: React.FC = () => {
    const [email, setEmail] = React.useState('')
    const [password, setPassword] = React.useState('')

    const push = useStore((state) => state.push)

    const setUserInfo = useGlobalStore((state) => state.setUserInfo)
    const navigate = useNavigate()

    const performLogin = async () => {
        try {
            const response = await fetch('/api/auth/login', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ ldEmail: { unEmail: email }, ldPassword: password })
            })
            if (!response.ok) {
                throw {
                    kind: Kind.Error,
                    message: `Failed to login: ${response.status}`,
                    detail: await response.text()
                }
            }
            const data: LoginResponse = await response.json()
            if (data.tag === 'LoginSuccess') {
                const user = data.contents as LoginSuccess
                setUserInfo({
                    sessionKey: user._uiSessionKey,
                    email: user._uiUserEmail.unEmail,
                    id: user._uiUserId,
                    name: user._uiUserName
                })
                navigate('/projects')
            } else {
                throw {
                    kind: Kind.Error,
                    message: 'Failed to login',
                    detail: data.contents
                }
            }
        } catch (error) {
            push(error as Config)
        }
    }

    return (
        <div className="p3">
            <h1 className="h2 m0 mb3">
                Login
            </h1>
            <table>
                <tbody>
                    <Row label="Email" name="email" value={email} onInput={setEmail} onEnter={performLogin} />
                    <Row label="Password" name="password" password value={password} onInput={setPassword} onEnter={performLogin} />
                    <Submit onClick={performLogin} />
                </tbody>
            </table>
            Not registered yet? <Link to="/signup" className="link">Sign up</Link>.<br/>
            <Link to="/forgot-password" className="link">Forgot your password?</Link>
        </div>
    )
}
