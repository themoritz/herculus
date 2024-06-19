import React from "react"
import { Row, Submit } from "../utils/Templates"
import { Link, useNavigate } from "react-router-dom"
import { Config, Kind, useStore } from "../Notifications"
import { useStore as useGlobalStore } from "../../store"

type Tag = 'SignupSuccess' | 'SignupFailed'

type Email = {
    unEmail: string
}

type SignupSuccess = {
    _uiSessionKey: string
    _uiUserEmail: Email
    _uiUserId: string
    _uiUserName: string
}

type SignupResponse = {
    tag: Tag,
    contents: string | SignupSuccess
}

export const SignUp: React.FC = () => {
    const [name, setName] = React.useState('')
    const [email, setEmail] = React.useState('')
    const [password, setPassword] = React.useState('')
    const [confirmPassword, setConfirmPassword] = React.useState('')

    const push = useStore((state) => state.push)
    const setUserInfo = useGlobalStore((state) => state.setUserInfo)
    const navigate = useNavigate()

    const [error, setError] = React.useState<string | null>(null)

    const performSignup = async () => {
        if (password !== confirmPassword)
            return setError('Passwords do not match.')

        if (name === '')
            return setError('Name is required.')

        if (password.length < 6)
            return setError('Password must be at least 6 characters long.')

        try {
            const response = await fetch('/api/auth/signup', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    suUserName: name,
                    suEmail: { unEmail: email },
                    suPassword: password,
                    suIntention: ''
                })
            })

            if (!response.ok) {
                throw {
                    kind: Kind.Error,
                    message: `Failed to sign up: ${response.status}`,
                    detail: await response.text()
                }
            }

            const data: SignupResponse = await response.json()

            if (data.tag === 'SignupSuccess') {
                const user = data.contents as SignupSuccess
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
                    message: 'Failed to sign up',
                    detail: data.contents
                }
            }
        } catch (error) {
            push(error as Config)
        }
    }

    return (
        <div className="p3">
            <h1 className="h2 m0 mb3">Sign Up</h1>
            <table>
                <tbody>
                    <Row label="Name" name="name" value={name} onInput={setName} onEnter={performSignup} />
                    <Row label="Email" name="email" value={email} onInput={setEmail} onEnter={performSignup} />
                    <Row label="Password" name="password" value={password} password onInput={setPassword} onEnter={performSignup} />
                    <Row label="Password (again)" name="confirmPassword" value={confirmPassword} password onInput={setConfirmPassword} onEnter={performSignup} />
                    <Submit onClick={performSignup} />
                </tbody>
            </table>
            {error}
            <p className="center">
                Please be aware that this is not a stable deployment or release. Bugs may occur, and you may lose your data.
            </p>
            <Link to="/login" className="link">Back to login</Link>
        </div>
    )
}
