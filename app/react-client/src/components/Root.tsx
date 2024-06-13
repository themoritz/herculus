import React from 'react';
import { Kind, Notifications, useStore } from './Notifications';
import { BrowserRouter, Route, Routes } from 'react-router-dom';
import { LoggedIn } from './LoggedIn';
import { LogIn } from './auth/LogIn';
import { SignUp } from './auth/SignUp';

export const Root: React.FC = () => {
    const push = useStore((state) => state.push)

    return (
        <BrowserRouter>
            <div>
                <Notifications />
                <Routes>
                    <Route path="/" element={<LoggedIn />} />
                    <Route path="/login" element={<LogIn />} />
                    <Route path="/signup" element={<SignUp />} />
                </Routes>
                <button onClick={() => push({ kind: Kind.Warn, message: 'Hello !' })}>Push</button>
            </div>
        </BrowserRouter>
    )
}
