import type { Post, PostData } from './post'


export async function getPosts(): Promise<Post[]> {
    return await Promise.all(Object.entries(import.meta.glob('/src/lib/posts/*.md'))
        .map(async ([path, resolver]) => {
            const { metadata: p, default: page } = await resolver() as MdsvexFile
            const slug = path.split('/').pop()?.slice(0, -3) ?? ''
            
            return { page,
                metadata: {
                    title : p.title ?? 'Unnamed Post',
                    date: p.date ?? Date(),
                    slug,
                    categories: p.categories ?? []
                }
            }
        })
    )
}

async function fetchPosts({ offset = 0, limit = 10, category = '' } = {}): Promise<{ posts: PostData[] }> {
    const allPosts = (await getPosts()).map(p => p.metadata)

    let posts = allPosts.sort((a, b) => Number(new Date(a.date)) - Number(new Date(b.date)))

    if (category) posts = posts.filter(p => p.categories.includes(category))

    if (offset) posts = posts.slice(offset)

    if (limit >= 0) posts = posts.slice(0, limit);

    return { posts }
}

export default fetchPosts
