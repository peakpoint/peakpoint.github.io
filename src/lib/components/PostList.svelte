<script lang="ts">
    import type { PostData } from '$lib/assets/post'
    import { formatDate } from '$lib/assets/dateOptions'

    export let posts: PostData[] = [];

</script>

<ul class='posts'>
    {#each posts as p}
        <li>
            <a href="/blog/{p.slug}">{p.title}</a>
            <p>{formatDate(p.date)}</p>
        </li>
    {/each}
</ul>

<style lang="scss">
    ul {
        padding-left: 0;

        li {
            list-style-type: none;

            a {
                font-size: 20px;
            }

            p {
                font-size: 14px;
                margin: 0.5rem 0;
                color: $dark-white;
            }
        }
    }
</style>
