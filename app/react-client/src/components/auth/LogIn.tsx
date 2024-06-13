import React from "react"

export const LogIn: React.FC = () => {
    const [email, setEmail] = React.useState('')
    const [password, setPassword] = React.useState('')
    const [error, setError] = React.useState('')

    return (
        <div className="p3">
            <h1 className="h2 m0 mb3">
                Login
            </h1>
            <table>
                <tbody>

                </tbody>
            </table>
            Not registered yet? <a href="/signup">Sign up</a>
        </div>
    )
}
