<script lang="ts">
	import type { ComponentType, SvelteComponentTyped } from 'svelte/internal'
    import { formatDate } from '$lib/assets/dateOptions'
    
    export let data

    const { title, date } = data.meta
    const PostContent = data.PostContent as unknown as ComponentType<SvelteComponentTyped>
</script>

<svelte:head>
    <title>{title}</title>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.7/dist/katex.min.css" integrity="sha384-3UiQGuEI4TTMaFmGIZumfRPtfKQ3trwQE2JgosJxCnGmQpL/lJdjpcHkaaFwHlcI" crossorigin="anonymous">
</svelte:head>

<h1 class="title">{title}</h1>
<p class="date">
    {formatDate(date)}
</p>
<hr>

<article>
    <div class='post'>
        <PostContent/>
    </div>
</article>

<style lang="scss">
    h1.title {
        font-size: 40px;
        color: $hl;
        text-decoration: underline;
        margin-bottom: 0;
    }

    p.date {
        color: $dark-white;
        margin-top: 0.5rem;
        margin-bottom: 1.5rem;
    }

    hr {
        border-color: $hl;
        border-style: dashed;
        border-width: 2px;
    }
</style>