import type { SvelteComponentDev } from "svelte/internal"

export interface PostData {
    title: string
    slug: string
    date: string
    categories: string[]
}

export interface Post {
    metadata: PostData
    page: SvelteComponentDev
}
