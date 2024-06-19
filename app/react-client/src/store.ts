import { create } from "zustand"

type UserInfo = {
    sessionKey: string
    email: string
    id: string
    name: string
}

interface State {
    userInfo: UserInfo | null
    setUserInfo: (userInfo: UserInfo) => void
}

export const useStore = create<State>((set) => ({
    userInfo: null,
    setUserInfo: (userInfo) => set({ userInfo })
}))
