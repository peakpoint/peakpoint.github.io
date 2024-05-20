<script lang="ts">
    import { fade } from "svelte/transition";
    import "../app.scss";

    export let data

    const nav: [string, string][] = [
        ["Home", "/"],
        ["Blog", "/blog"],
        ["Contact", "/contact"],
    ];
</script>

<nav>
    <div>
        {#each nav as [name, dir]}
            <a href={dir}
            >{name}</a>
        {/each}
    </div>
</nav>

{#key data.pathname}
    <div class='layout'
        in:fade|global={{ duration: 100, delay: 150 }}
        out:fade|global={{ duration: 100 }}>
        <slot />
    </div>
{/key}

<style lang="scss">
    .layout {
        margin: 4rem auto;
        padding: 0 2rem;
        max-width: 50rem;
    }

    nav {
        div {
            width: 100%;
            display: inline-block;
            // padding: 0 6rem;
            text-align: center;
            border-bottom: 2px solid $gray;

            a {
                display: inline-block;
                list-style-type: none;
                padding: 1rem 2rem;
                
                font-size: 18px;
                color: white;
                text-decoration: none;

                &:hover {
                    background-color: $gray;
                }
            }
        }
    }
</style>
