<script lang="ts">
    import PostList from "$lib/components/PostList.svelte"
    import type { PostData } from '$lib/assets/post'

    export let data: { posts: PostData[] };
</script>

<svelte:head>
    <title>Blog</title>
</svelte:head>

<h1>Blog landing page</h1>

<PostList posts={data.posts}/>
