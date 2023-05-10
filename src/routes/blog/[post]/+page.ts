import { error } from "@sveltejs/kit";
import type { PageLoad } from "./$types";
import { getPosts } from "$lib/assets/fetchPosts";

export const load: PageLoad = async ({ params }) => {
    const posts = await getPosts()
    
    const post = posts.find(p => p.metadata.slug == params.post)

    if (!post) throw error(404)
    
    return {
        PostContent: post.page,
        meta: post.metadata
    }
}
