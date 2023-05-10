// See https://kit.svelte.dev/docs/types#app
// for information about these interfaces
declare global {
	namespace App {
		// interface Error {}
		// interface Locals {}
		// interface PageData {}
		// interface Platform {}
	}

	interface MdsvexFile {
		default: import('svelte/internal').SvelteComponentDev;
		metadata: Record<string, any>;
	}

	type MdsvexResolver = () => Promise<MdsvexFile>;
}

export {};
